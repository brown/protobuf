# Protobuf

Protobuf is a Common Lisp implementation of Google's protocol buffers.

The protobuf repository contains a compiler from .proto files to Lisp and the
run time support needed by the generated Lisp code.  Not all protocol buffer
features are implemented, but the code works well enough that serialization
tests pass.

The protobuf code has been tested with ABCL, CCL, CLISP, Lispworks, and SBCL.
In the past it has also worked with Allegro Common Lisp, but I have not tested
it recently.

The installation instructions below work with release 3.6.1 of Google's
protocol buffer compiler.

## Installation

1. The Common Lisp protocol buffer compiler is implemented as a plugin to
Google's protocol buffer compiler, so you must first build and install Google's
compiler, which is called protoc.  The required code and documentation are
available [here](https://github.com/google/protobuf).

The steps should be something like:

```
cd /tmp
git clone https://github.com/google/protobuf google-protobuf
cd google-protobuf
./autogen.sh
./configure --prefix=~/local/software/package/google-protobuf-3.6.1
make
make check
make install
```

2. Clone the Lisp protobuf Git repository to get a local copy and compile
`protoc-gen-lisp`, the Common Lisp protocol buffer plugin.  The steps required
for Linux are:

```
cd /tmp
git clone git://github.com/brown/protobuf.git

cd protobuf/protoc/lisp

# Copy strutil.h from the Google's protocol buffer compiler source
# directory:
cp /tmp/google-protobuf/src/google/protobuf/stubs/strutil.h .

# Change INSTALL_ROOT and PROTOC_ROOT in Makefile.  INSTALL_ROOT indicates
# where protoc-gen-lisp should be installed.  PROTOC_ROOT indicates where
# you installed Google's protobuf compiler when you compiled it in step 1.

# Compile and install the Lisp protoc plugin.
make install
```

3. Download and install the Common Lisp packages that protobuf depends on.
First, you'll need ASDF, but it comes pre-installed in most Common Lisp
distributions.  You'll also need com.google.base, which is available via
Quicklisp.  To run all the tests, you'll need the Stefil testing package and
its dependencies: hu.dwim.stefil, hu.dwim.asdf, alexandria.  All of these can
easily be downloaded using Quicklisp.

If you're not using Allegro, CLisp, or SBCL, you may need trivial-utf8, again
available via Quicklisp.

4. Make protobuf and its dependencies available to ASDF.  There are several
ways to do this and you should consult the ASDF documentation to determine what
will work best for you.  If you've downloaded dependencies using Quicklisp,
then ASDF will automatically know about them.

ASDF and its manual are available [here](http://common-lisp.net/project/asdf).

On my system, I tell ASDF where to find the protobuf system files by creating a
file called `source-registry.conf` in directory `/home/brown/.config/common-lisp`
with the following contents:

```
(:source-registry
 (:tree "/home/brown/src/protobuf/")
 :inherit-configuration)
```

5. Make sure ASDF can execute Google's protocol buffer compiler and the Common
Lisp plugin.  Both `protoc` and `protoc-gen-lisp` must be installed in
directories that appear in your shell's `PATH` environment variable.

6. Compile and load all the protobuf code:

```
(asdf:load-system 'protobuf)
```

7. Optionally, load and run all the tests:

```
(asdf:test-system 'varint)
(asdf:test-system 'protobuf)
```

8. Compile and run the example code, which shows how to incorporate protocol
buffer definition files into your own projects:

```
(asdf:load-system 'protobuf-example)

(in-package address-book)
(add-person :id 100
            :name "Robert Brown"
            :email-address "brown@foo.com"
            :phone-numbers '((home . "718-555-1212")
                             (work . "212-589-1212")
                             (mobile . "917-555-1212")))
(list-people)
```

## Bugs

Please report bugs and send suggestions on GitHub or contact me directly.  My
email is robert.brown at the mail hosting site gmail.com.
