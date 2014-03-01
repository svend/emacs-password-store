# Emacs password-store

This package provides functions for working with pass ("the
standard Unix password manager").

http://www.zx2c4.com/projects/password-store

## Generate password using `pass`

	$ pass generate example 10
	[master 68eaa05] Added generated password for example to store.
	 1 file changed, 0 insertions(+), 0 deletions(-)
	 create mode 100644 example.gpg
	The generated password to example is:
	O#d5!{shRL

## Get password in Emacs

Lisp:

	(password-store-get "example") ; Returns "O#d5!{shRL"

Interactive:

	M-x password-store-copy
	Password entry: example
	Copied example to the kill ring. Will clear in 45 seconds.
	Password cleared.
