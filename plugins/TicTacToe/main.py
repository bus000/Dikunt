#!/usr/bin/python3

import json
import random
import re
import sys
import time

# Setup.
botnick = sys.argv[1]
channel = sys.argv[2]

# Mapping from nicknames to games.
games = {}

# Patterns.
sp = '[ \\t]*'
ps = '[ \\t]+'
help_pattern = re.compile('^' + sp + botnick + ':' + ps + 'tictactoe' + ps +\
    'help' + sp + '$')
start_pattern = re.compile('^' + sp + botnick + ':' + ps + 'tictactoe' + ps +\
    'start' + sp + '$')
play_pattern = re.compile('^' + sp + '[1-9]' + sp + '$')

# Represent a TicTacToe game with a user. The board layout is show below.
#
# -------------
# | 1 | 2 | 3 |
# -------------
# | 4 | 5 | 6 |
# -------------
# | 7 | 8 | 9 |
# -------------
class TicTacToeGame:

    cross = 1
    circle = 2

    def __init__(self, nick):
        self.start = time.time()
        self.nick = nick
        self.board = {}

        print('To play, simply write the number of the move you want to make')

        self.show_board()

    def make_move(self, move):
        if not self.legal_move(move):
            print('Error: not a legal move, terminating game.')
            del games[self.nick]
        else:
            self.board[move] = self.cross
            self.make_bot_move()

            if self.won() is not None:
                self.show_result(self.won())
                del games[self.nick]
            else:
                self.show_board()

    def show_result(self, winner):
        self.show_board()
        if winner == self.circle:
            print('Seems like I win this time')
        elif winner == self.cross:
            print('Congratulations, you won')

    def won(self):
        crosses = [key for key, value in self.board.items() if value == self.cross]
        circles = [key for key, value in self.board.items() if value == self.circle]

        winning_configurations = [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8],
                [3,6,9], [1,5,9], [3,5,7]]

        for conf in winning_configurations:
            if all(key in crosses for key in conf):
                return self.cross
            elif all(key in circles for key in conf):
                return self.circle

        return None

    def make_bot_move(self):
        legal_moves = [key for key in range(1, 10) if key not in self.board]
        if len(legal_moves) > 0:
            self.board[random.choice(legal_moves)] = self.circle

    def legal_move(self, move):
        if move > 9 or move < 1:
            return False
        elif move in self.board:
            return False
        else:
            return True

    def show_board(self):
        print('---------------')

        for i in range(1, 10):
            if i in self.board and self.board[i] == self.cross:
                print('|', 'X', '|', end='')
            elif i in self.board and self.board[i] == self.circle:
                print('|', 'O', '|', end='')
            else:
                print('|', i, '|', end='')

            if i % 3 == 0:
                print('\n---------------')

def play_game(mes, nick):
    games[nick] = TicTacToeGame(nick)

def play_game_move(mes, nick, move):
    if nick in games and time.time() - games[nick].start < 3600:
        games[nick].make_move(move)
    elif nick in games:
        print('Game timed out.')
        del games[nick]

def display_help():
    print(botnick + ": tictactoe help - Display this message.")
    print(botnick + ": tictactoe start - Start a game of TicTacToe.")

def handle_message(mes, nick):
    if help_pattern.match(mes):
        display_help()
    elif start_pattern.match(mes):
        play_game(mes, nick)
    elif play_pattern.match(mes):
        play_game_move(mes, nick, int(mes))

# Handle input which comes as lines of json.
for line in sys.stdin:
    message = json.loads(line)

    if message['command'] == 'PRIVMSG':
        handle_message(message['message'], message['ircUser']['nickname'])
