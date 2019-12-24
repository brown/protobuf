
# Directory into which the Lisp protocol buffer plugin will be installed.
INSTALL_ROOT = $(HOME)/local/software/package/protoc-gen-lisp

# Directory where Google's protocol buffer compiler is installed.
PROTOC_ROOT = $(HOME)/local/software/package/google-protobuf

PROTOC_INCLUDE = $(PROTOC_ROOT)/include
PROTOC_LIB = $(PROTOC_ROOT)/lib

OFILES = enum.o enum_field.o field.o file.o generator.o helpers.o message.o message_field.o \
         primitive_field.o protoc_gen_lisp.o service.o string_field.o

CXXFLAGS = -std=c++11 -I$(PROTOC_INCLUDE) -I.
LIBS = -Wl,-L$(PROTOC_LIB) -Wl,-rpath -Wl,$(PROTOC_LIB) -lprotoc -lprotobuf -lpthread

protoc-gen-lisp: $(OFILES) Makefile
	$(CXX) -o protoc-gen-lisp $(OFILES) $(LIBS)

install: protoc-gen-lisp
	mkdir -p $(INSTALL_ROOT)/bin
	cp protoc-gen-lisp $(INSTALL_ROOT)/bin

clean:
	rm -f $(OFILES) protoc-gen-lisp


enum.o: enum.h helpers.h
enum_field.o: enum_field.h field.h helpers.h
field.o: enum_field.h field.h message_field.h primitive_field.h string_field.h
file.o: file.h enum.h helpers.h message.h field.h service.h
generator.o: file.h generator.h helpers.h
helpers.o: helpers.h
message.o: enum.h field.h helpers.h message.h
message_field.o: field.h helpers.h
primitive_field.o: field.h primitive_field.h helpers.h
protoc_gen_lisp.o: generator.h
service.o: helpers.h service.h
string_field.o: field.h helpers.h string_field.h
