#!/usr/bin/python3

# Copyright (c) 2015 Matthew Earl
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#     The above copyright notice and this permission notice shall be included
#     in all copies or substantial portions of the Software.
#
#     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
#     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
#     NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#     DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
#     OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
#     USE OR OTHER DEALINGS IN THE SOFTWARE.

"""

Most of this code is taken from https://github.com/matthewearl/faceswap

"""

import argparse
import cv2
import dlib
import json
import numpy
import sys
import os
import urllib.request
import dropbox
import hashlib
import sys
from parsy import generate, string, regex, eof, alt, Parser, ParseError
from tempfile import NamedTemporaryFile

# Parse command line arguments.
parser = argparse.ArgumentParser(description='None')
parser.add_argument('botnick', metavar='N', type=str,
        help='Nickname of the bot')
parser.add_argument('channel', metavar='N', type=str,
        help='Channel we run on')
parser.add_argument('plugins', metavar='N', type=str,
        help='Plugins running')
parser.add_argument('--predictor-path', dest='pred_path',
        help='Path to predictor')
parser.add_argument('--mark-image-path', dest='mark_path',
        help='Path to image of The Mark')
parser.add_argument('--dropbox-token', dest='dropbox_token',
        help='Token to connect to dropbox')

args, unknown = parser.parse_known_args()

# Setup connection to dropbox.
dbx = dropbox.Dropbox(args.dropbox_token)
dbx.users_get_current_account()

HELP_REQUEST = 0
FIND_MARK_REQUEST = 1

PREDICTOR_PATH = args.pred_path
SCALE_FACTOR = 1
FEATHER_AMOUNT = 11

FACE_POINTS = list(range(17, 68))
MOUTH_POINTS = list(range(48, 61))
RIGHT_BROW_POINTS = list(range(17, 22))
LEFT_BROW_POINTS = list(range(22, 27))
RIGHT_EYE_POINTS = list(range(36, 42))
LEFT_EYE_POINTS = list(range(42, 48))
NOSE_POINTS = list(range(27, 35))
JAW_POINTS = list(range(0, 17))

# Points used to line up the images.
ALIGN_POINTS = (LEFT_BROW_POINTS + RIGHT_EYE_POINTS + LEFT_EYE_POINTS +
                               RIGHT_BROW_POINTS + NOSE_POINTS + MOUTH_POINTS)

# Points from the second image to overlay on the first. The convex hull of each
# element will be overlaid.
OVERLAY_POINTS = [
    LEFT_EYE_POINTS + RIGHT_EYE_POINTS + LEFT_BROW_POINTS + RIGHT_BROW_POINTS,
    NOSE_POINTS + MOUTH_POINTS,
]

# Amount of blur to use during colour correction, as a fraction of the
# pupillary distance.
COLOUR_CORRECT_BLUR_FRAC = 0.6

detector = dlib.get_frontal_face_detector()
predictor = dlib.shape_predictor(PREDICTOR_PATH)

# Represents a request comming from a user. A request can either be a
# HELP_REQUEST or a FIND_MARK_REQUEST with an associated URL.
class Request:
    def __init__(self, request_type, url=None):
        self.request_type = request_type
        self.url = url

# Handle a Request. None is an allowed value and will be returned.
def handle_request(req):
    if req == None:
        pass
    elif req.request_type == HELP_REQUEST:
        give_help(args.botnick)
    elif req.request_type == FIND_MARK_REQUEST:
        find_mark(args.botnick, req.url)

# Print help message to stdout.
def give_help(botnick):
    print(botnick + ': mark help - Display this message')
    print(botnick + ": find mark <url> - Locate The Mark in the picture given")

# Download the file pointed to by the URL and locate The Mark in the image. If
# it is not an image or the file is to large an error message will be printed.
def find_mark(botnick, url):
    with NamedTemporaryFile() as f1, NamedTemporaryFile(suffix='.png') as f2:
        try:
            print('Jeg leder efter The Mark')
            response = urllib.request.urlopen(url)
            if int(response.getheader("Content-Length")) > 1000000000:
                raise Exception
            f1.write(response.read())

            replace_face(f1.name, args.mark_path, f2.name)
            file_url = upload_file(f2.name)
            print('The Mark er fundet ' + file_url)
        except TooManyFaces:
            print('Der er for mange ansigter i dit billede')
        except NoFaces:
            print('Jeg kunne ikke finde The Mark')
        except Exception as e:
            print('Filen er for stor eller kan ikke hentes')
            print(e)

# Upload file to dropbox and return the URL to that file.
def upload_file(fname):
    with open(fname, 'rb') as f:
        content = f.read()
        dbname = "/{}.png".format(hashlib.md5(content).hexdigest())

        dbx.files_upload(content, dbname)

        dbx.sharing_create_shared_link(dbname, True)
        return dbx.sharing_get_shared_links(dbname).links[0].url

# Takes a message from the IRC channel and parse it to the Request class.
def parse_request(message):
    try:
        parser = Parser(request)
        return parser.parse(message)
    except ParseError:
        return None

# Parser generator that parses a request.
@generate
def request():
    req = yield alt(help_request, find_mark_request)
    return req

# Parser generator that parses a help request.
@generate
def help_request():
    yield regex(r' ').many()
    yield string(args.botnick + ":")
    yield regex(r' ').at_least(1)
    yield string('mark')
    yield regex(r' ').at_least(1)
    yield string('help')
    yield regex(r' ').many()
    yield eof

    return Request(HELP_REQUEST)

# Parser generator that parses a "find mark" request.
@generate
def find_mark_request():
    yield regex(r' ').many()
    yield string(args.botnick + ":")
    yield regex(r' ').at_least(1)
    yield string('find')
    yield regex(r' ').at_least(1)
    yield string('mark')
    yield regex(r' ').at_least(1)
    url = yield regex(r"[^ \t]").at_least(1)
    yield regex(r' ').many()
    yield eof

    return Request(FIND_MARK_REQUEST, "".join(url))

class TooManyFaces(Exception):
    pass

class NoFaces(Exception):
    pass

def get_landmarks(im):
    rects = detector(im, 1)

    # if len(rects) > 1:
        # raise TooManyFaces
    if len(rects) == 0:
        raise NoFaces

    return numpy.matrix([[p.x, p.y] for p in predictor(im, rects[0]).parts()])

def annotate_landmarks(im, landmarks):
    im = im.copy()
    for idx, point in enumerate(landmarks):
        pos = (point[0, 0], point[0, 1])
        cv2.putText(im, str(idx), pos,
                    fontFace=cv2.FONT_HERSHEY_SCRIPT_SIMPLEX,
                    fontScale=0.4,
                    color=(0, 0, 255))
        cv2.circle(im, pos, 3, color=(0, 255, 255))
    return im

def draw_convex_hull(im, points, color):
    points = cv2.convexHull(points)
    cv2.fillConvexPoly(im, points, color=color)

def get_face_mask(im, landmarks):
    im = numpy.zeros(im.shape[:2], dtype=numpy.float64)

    for group in OVERLAY_POINTS:
        draw_convex_hull(im, landmarks[group], color=1)

    im = numpy.array([im, im, im]).transpose((1, 2, 0))

    im = (cv2.GaussianBlur(im, (FEATHER_AMOUNT, FEATHER_AMOUNT), 0) > 0) * 1.0
    im = cv2.GaussianBlur(im, (FEATHER_AMOUNT, FEATHER_AMOUNT), 0)

    return im

def transformation_from_points(points1, points2):
    """
    Return an affine transformation [s * R | T] such that:

        sum ||s*R*p1,i + T - p2,i||^2

    is minimized.

    """
    # Solve the procrustes problem by subtracting centroids, scaling by the
    # standard deviation, and then using the SVD to calculate the rotation. See
    # the following for more details:
    #   https://en.wikipedia.org/wiki/Orthogonal_Procrustes_problem

    points1 = points1.astype(numpy.float64)
    points2 = points2.astype(numpy.float64)

    c1 = numpy.mean(points1, axis=0)
    c2 = numpy.mean(points2, axis=0)
    points1 -= c1
    points2 -= c2

    s1 = numpy.std(points1)
    s2 = numpy.std(points2)
    points1 /= s1
    points2 /= s2

    U, S, Vt = numpy.linalg.svd(points1.T * points2)

    # The R we seek is in fact the transpose of the one given by U * Vt. This
    # is because the above formulation assumes the matrix goes on the right
    # (with row vectors) where as our solution requires the matrix to be on the
    # left (with column vectors).
    R = (U * Vt).T

    return numpy.vstack(
            [numpy.hstack(((s2 / s1) * R, c2.T - (s2 / s1) * R * c1.T)),
                numpy.matrix([0., 0., 1.])])

def read_im_and_landmarks(fname):
    im = cv2.imread(fname, cv2.IMREAD_COLOR)
    im = cv2.resize(im, (im.shape[1] * SCALE_FACTOR,
        im.shape[0] * SCALE_FACTOR))
    s = get_landmarks(im)

    return im, s

def warp_im(im, M, dshape):
    output_im = numpy.zeros(dshape, dtype=im.dtype)
    cv2.warpAffine(im, M[:2], (dshape[1], dshape[0]), dst=output_im,
            borderMode=cv2.BORDER_TRANSPARENT, flags=cv2.WARP_INVERSE_MAP)
    return output_im

def correct_colours(im1, im2, landmarks1):
    blur_amount = COLOUR_CORRECT_BLUR_FRAC * numpy.linalg.norm(
            numpy.mean(landmarks1[LEFT_EYE_POINTS], axis=0) -
            numpy.mean(landmarks1[RIGHT_EYE_POINTS], axis=0))
    blur_amount = int(blur_amount)
    if blur_amount % 2 == 0:
        blur_amount += 1
    im1_blur = cv2.GaussianBlur(im1, (blur_amount, blur_amount), 0)
    im2_blur = cv2.GaussianBlur(im2, (blur_amount, blur_amount), 0)

    # Avoid divide-by-zero errors.
    im2_blur += (128 * (im2_blur <= 1.0)).astype(im2_blur.dtype)

    return (im2.astype(numpy.float64) * im1_blur.astype(numpy.float64) /
            im2_blur.astype(numpy.float64))

# Performs face replacement. Loads the mark and source files and replaces the
# face in the source with the face of The Mark. The result is written to dest.
def replace_face(mark_file, source, dest):
    im1, landmarks1 = read_im_and_landmarks(mark_file)
    im2, landmarks2 = read_im_and_landmarks(source)

    M = transformation_from_points(landmarks1[ALIGN_POINTS],
            landmarks2[ALIGN_POINTS])

    mask = get_face_mask(im2, landmarks2)
    warped_mask = warp_im(mask, M, im1.shape)
    combined_mask = numpy.max([get_face_mask(im1, landmarks1), warped_mask],
            axis=0)

    warped_im2 = warp_im(im2, M, im1.shape)
    warped_corrected_im2 = correct_colours(im1, warped_im2, landmarks1)

    output_im = im1 * (1.0 - combined_mask) + warped_corrected_im2 *\
            combined_mask

    cv2.imwrite(dest, output_im)

# Main: loop through lines in stdin, parse requests and handle those requests.
for line in sys.stdin:
    message = json.loads(line)

    if message['command'] == 'PRIVMSG':
        req = parse_request(message['message'])
        handle_request(req)
