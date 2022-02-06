
PROTOBUF_ROOT := ${HOME}/local/software/source/google-protobuf
TEST_RUNNER := ${PROTOBUF_ROOT}/conformance/conformance-test-runner

CONFORMANCE_DEPS := \
    $(TEST_RUNNER) \
    protobuf-conformance.asd \
    conformance-lisp.lisp \
    build-conformance-sbcl.lisp \
    conformance.proto \
    google/protobuf/test_messages_proto2.proto \
    google/protobuf/any.proto \
    google/protobuf/duration.proto \
    google/protobuf/field_mask.proto \
    google/protobuf/struct.proto \
    google/protobuf/timestamp.proto \
    google/protobuf/wrappers.proto \
    google/protobuf/test_messages_proto3.proto

.PHONY: test_lisp

all: test_lisp

test_lisp: conformance-sbcl
	${TEST_RUNNER} \
	    --enforce_recommended \
	    --failure_list failure_list_lisp.txt \
	    --text_format_failure_list text_format_failure_list_lisp.txt \
	    ./conformance-sbcl

conformance-sbcl: $(CONFORMANCE_DEPS)
	sbcl < build-conformance-sbcl.lisp
