#!/usr/bin/python3

import configparser
import discord
import obclient
from sys import argv, stderr


def print_usage():
    print("Usage: {} host [port]\n  Default port is 4563.".format(argv[0]))


# SETUP
config = configparser.ConfigParser()
config.read('discordbot.conf')

TOKEN = config['Default']['API_KEY']

ob_host = None
ob_port = 4563

if len(argv) < 2:
    print_usage()
    exit(1)

ob_host = argv[1]

if len(argv) > 2:
    try:
        ob_port = int(argv[2])
    except ValueError:
        print('Error: Bad port: "{}"\n'.format(argv[2]), file=stderr)
        print_usage()
        exit(1)


oxonbot_client = obclient.OxonbotClient("discord", ob_host, ob_port)
if oxonbot_client.Connect():
    print('Successfully connected to Oxonbot server.')
else:
    print('Error: Couldn\'t connect to Oxonbot server.', file=stderr)


discord_client = discord.Client()
# END OF SETUP


@discord_client.event
async def on_message(message):
    if message.author == discord_client.user:
        return

    # message.channel can be Channel, PrivateChannel, Object
    channel = None
    if isinstance(message.channel, discord.Channel) and message.channel.name:
        channel = '#' + message.channel.name
    else:
        channel = "_default_"

    # message.server can be Server or None
    server = None
    if isinstance(message.server, discord.Server):
        server = message.server.name
    else:
        server = "_default_"

    # path arg should be: server:#channel
    if message.content.startswith('.'):
        resp = oxonbot_client.SendQuery(server + ':' + channel,
                                        message.author.name,
                                        message.content[1:])
        if resp:
            await discord_client.send_message(message.channel, resp.text)


@discord_client.event
async def on_ready():
    print('Logged in as')
    print(discord_client.user.name)
    print(discord_client.user.id)
    print('------')

discord_client.run(TOKEN)
