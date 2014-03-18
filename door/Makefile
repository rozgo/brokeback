PROJECT = door
 
DEPS = cowboy lhttpc eredis
dep_cowboy = pkg://cowboy master
dep_lhttpc = https://github.com/esl/lhttpc.git master
dep_eredis = https://github.com/wooga/eredis.git

ifeq ($(ENV),)
	APP_ENV ?= dev
else
	APP_ENV = $(ENV)
endif

RELX_URL = http://tools.beyondgames.io/relx
RELX_OPTS = --sys_config $(PWD)/config/$(APP_ENV).config

include erlang.mk
