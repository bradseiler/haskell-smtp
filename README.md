haskell-smtp
============

A toy SMTP server written in Haskell
by Brad Seiler
written for Computer Science 152 (Programming Languages) at Harvard University
Spring 2008

OVERVIEW
--------

This code implements a simple server for receiving SMTP transmissions from [RFC821](http://tools.ietf.org/html/rfc821) compliant SMTP clients.  It listens on a specified port for connections and then spawns transaction threads for each incoming request.  Each request is handled according to the basic requirements laid out in RFC821.  If a valid e-mail message is received, it is save in an output file (`msgs.txt`).  All transactions, successful and unsuccessful, are logged (`log.txt`).

COMPILING
---------

For optimal performance, the server should be compiled with GHC using the following command:

`ghc --make Main.hs -o smtp_sever`

where `smtp_server` is the desired name of the executable.  Code developed and tested using GHC 6.6.

CUSTOMIZING
-----------

The file `Main.hs` contains a variable for setting the port to listen on for incoming connections.  Low numbered ports may require root privileges, so choose carefully. 

The file `Reply.hs` contains several variables at the top of the file which determine which e-mail addresses are accepted by the server. The server is designed only to accept email for its domain and for users it recognizes, since it cannot currently relay messages.

COMING SOON
-----------

This implementation, while functional, has room for expansion.  The next version should be able to relay messages to other SMTP servers. Future version should include a configuration file and upgrade compliance to [RFC2821](http://tools.ietf.org/html/rfc2821).
