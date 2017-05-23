#!/usr/bin/python

import fbchat
import sys
import threading
import re

botnick = sys.argv[1]
channel = sys.argv[2]
email = None
password = None

def display_help():
    print(botnick + ': facebooklurker help - Display this message.')
    print('Otherwise shows messages sent to configured bot on Facebook.')

def helper_writer():
    for line in sys.stdin:
        message = json.loads(line)

        if message['command'] == 'PRIVMSG' and help_pattern.match(message['message']):
            display_help()

class LurkerBot(fbchat.Client):

    def __init__(self, email, password):
        fbchat.Client.__init__(self, email, password, None, None)

    def on_message_new(self, mid, author_id, message, metadata, recipient_id,
            thread_type):
        self.markAsDelivered(author_id, mid)
        self.markAsRead(author_id)

        print('Facebook message:', message)

# Parse arguments.
for i in range(3, len(sys.argv)):
    if sys.argv[i].startswith('--facebook-email='):
        email = sys.argv[i].split('--facebook-email=', 1)
    if sys.argv[i].startswith('--facebook-password='):
        password = sys.argv[i].split('--facebook-password=', 1)

# Patterns.
sp = '[ \\t]*'
ps = '[ \\t]+'
help_pattern = re.compile('^' + sp + botnick + ':' + ps + 'facebooklurker' +\
    ps + 'help' + sp + '$')


# Start thread listening to IRC chat for help messages.
t = threading.Thread(target=helper_writer)
t.start()

if email == None or password == None:
    print('Did not get email and password to Facebook.')
    t.join()
else:
    bot = LurkerBot(email, password)
    bot.listen()
    t.join()
