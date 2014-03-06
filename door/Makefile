PROJECT = door
 
DEPS = cowboy
dep_cowboy = pkg://cowboy master

ifeq ($(ENV),)
	APP_ENV ?= dev
else
	APP_ENV = $(ENV)
endif

RELX_URL = http://tools.beyondgames.io/relx
RELX_OPTS = --sys_config $(PWD)/config/$(APP_ENV).config

include erlang.mk

update-config:
	cp $(CURDIR)/env/$(APP_ENV).config $(CURDIR)/