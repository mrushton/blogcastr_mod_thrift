#########################################################
# Makefile: Makefile for mod_thrift.
#
# Author: Matt Rushton
# Date: 8/11/09
# Copyright 2009 Blogcastr, Inc.
#########################################################

THRIFT_DIR = ../../thrift/
EJABBERD_DIR = /home/mrushton/tmp/ejabberd-modules/trunk/src/
EJABBERD_EBIN_DIR = /usr/lib/ejabberd/ebin/

.PHONY = all clean install

all: mod_thrift.beam

#AS DESIGNED: assume thrift files are properly generated
mod_thrift.beam: mod_thrift.erl
	erlc -I ${THRIFT_DIR}/gen-erl/ -I ${EJABBERD_DIR}  mod_thrift.erl

install: mod_thrift.beam
	sudo cp mod_thrift.beam ${EJABBERD_EBIN_DIR}

clean:
	rm -rf mod_thrift.beam
