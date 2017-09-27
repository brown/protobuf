
INSTALL_ROOT = /local/software/package/protoc-gen-lisp

PROTOC_ROOT = /local/software/package/protobuf
PROTOC_INCLUDE = $(PROTOC_ROOT)/include
PROTOC_LIB = $(PROTOC_ROOT)/lib

HFILES = enum.h enum_field.h field.h file.h generator.h helpers.h message.h message_field.h primitive_field.h service.h string_field.h
OFILES = enum.o enum_field.o field.o file.o generator.o helpers.o message.o message_field.o primitive_field.o service.o string_field.o protoc_gen_lisp.o

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
enum_field.o: enum_field.h helpers.h field.h
field.o: field.h primitive_field.h string_field.h field.h enum_field.h message_field.h
file.o: file.h enum.h service.h helpers.h message.h field.h
generator.o: generator.h file.h field.h helpers.h
helpers.o: helpers.h
message.o: message.h enum.h helpers.h field.h
message_field.o: field.h helpers.h
primitive_field.o: primitive_field.h field.h helpers.h
service.o: service.h helpers.h
string_field.o: string_field.h field.h helpers.h

protoc_gen_lisp.o: generator.h

# XXXXXXX
strutil.o: strutil.h
