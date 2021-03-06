Keeper
======

Introduction
------------

*Keeper* is a set of tools to manage collections of SSH keys. With the common
method (the classical flat file ./ssh/authorized\_keys) it started to be a mess
to manage an large quantity of users and keys.
With *keeper*, the idea is quiet simple: use the power of database associated
of the rich language *haskell* to provide an easy way to manage SSH keys.

How it works
------------

It's simple, since *OpenSSH version 1.6* it's possible to provide a command to
*ssh-daemon* to show the list of authorized keys to connect to a user.
So, each time a connection will be required through SSH. The login will be
given to this command and the binary will do the work for you.

Installation
============

Keeper
------

    nicolas@typed.io$ cabal configure -f executable
    nicolas@typed.io$ cabal build

  TODO: install the binaries in /usr/bin

Configure OpenSSH
-----------------

Create a user without passwd:

    nicolas@typed.io# sudo useradd -s /usr/sbin/nologin sshkey

Insert the thow lines in /etc/ssh/sshd\_config:

    AuthorizedKeysCommand /usr/bin/check\_authorizedkeys
    AuthorizedKeysCommandUser sshkey

Flat version
------------

The actual version of Keeper implement a json file database. This database is
expected to be in the user's *.ssh* directory. A quick action to ensure
everything work could be to change the access right:

    nicolas@typed.io$ chmod +x ~nicolas/.ssh

A better way could be to change the group right and allow the user "sshkey" to
access this directory.
