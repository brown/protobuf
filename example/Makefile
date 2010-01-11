
# Build Lisp source files corresponding to protocol buffer messages.
# Redefine PROTOC if you have installed the protocol buffer compiler
# elsewhere.


PROTOC = ../google-protobuf/src/protoc

%.lisp: %.proto
	$(PROTOC) --proto_path=. --lisp_out=. $<

PROTO_IMPLS = addressbook.lisp

.PHONY: all clean

all: $(PROTOC) $(PROTO_IMPLS)

clean:
	rm -f $(PROTO_IMPLS)
