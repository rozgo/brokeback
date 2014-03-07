PROJECT = door
 
DEPS = cowboy decorators statman newrelic lhttpc jiffy
dep_cowboy = pkg://cowboy master
dep_decorators = https://github.com/beyondgames/erlang_decorators.git master
dep_statman = https://github.com/beyondgames/statman.git master
dep_newrelic = https://github.com/beyondgames/newrelic-erlang.git master
dep_lhttpc = https://github.com/esl/lhttpc.git master
dep_jiffy = https://github.com/davisp/jiffy.git master

ifeq ($(ENV),)
	APP_ENV ?= dev
else
	APP_ENV = $(ENV)
endif

RELX_URL = http://tools.beyondgames.io/relx
RELX_OPTS = --sys_config $(PWD)/config/$(APP_ENV).config

include erlang.mk
