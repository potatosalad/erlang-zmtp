# -*- mode: makefile; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
# vim: ts=4 sw=4 ft=makefile noet
# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2014, Andrew Bennett <potatosaladx@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

.PHONY: all deps app rel docs tests clean distclean help

REBAR_MK_VERSION = 1

# Core configuration.

PROJECT ?= $(notdir $(CURDIR))
PROJECT := $(strip $(PROJECT))

# Verbosity.

V ?= 0

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose = $(gen_verbose_$(V))

# Core targets.

all:: deps app rel

clean::
	$(gen_verbose) rm -f erl_crash.dump

distclean:: clean

# Core functions.

define core_http_get
	wget --no-check-certificate -O $(1) $(2)|| rm $(1)
endef

# Copyright (c) 2014, Andrew Bennett <potatosaladx@gmail.com>
# This file is part of rebar.mk and subject to the terms of the ISC License.

.PHONY: clean-deps compile-deps distclean-deps fetch-deps

# Core targets.

deps:: fetch-deps compile-deps

distclean:: distclean-deps

# Deps related targets.

clean-deps: rebar
	$(rebar) -r skip_apps=$(PROJECT) clean

compile-deps: rebar
	$(rebar) -r skip_apps=$(PROJECT) compile

distclean-deps: rebar
	$(rebar) delete-deps

fetch-deps: rebar
	$(rebar) get-deps
	$(rebar) check-deps

# Copyright (c) 2014, Andrew Bennett <potatosaladx@gmail.com>
# This file is part of rebar.mk and subject to the terms of the ISC License.

.PHONY: clean-app

# Configuration.

# Verbosity.

# Core targets.

app:: compile-app

clean:: clean-app

# App related targets.

clean-app: rebar
	$(rebar) -r skip_deps=true clean

compile-app: rebar
	$(rebar) skip_deps=true compile

# Copyright (c) 2014, Andrew Bennett <potatosaladx@gmail.com>
# This file is part of rebar.mk and subject to the terms of the ISC License.

.PHONY: distclean-edoc

# Core targets.

docs:: distclean-edoc rebar
	$(rebar) -r skip_deps=true doc

distclean:: distclean-edoc

# Plugin-specific targets.

distclean-edoc:
	$(gen_verbose) rm -f doc/*.css doc/*.html doc/*.png doc/edoc-info

# Copyright (c) 2014, Andrew Bennett <potatosaladx@gmail.com>
# This file is part of rebar.mk and subject to the terms of the ISC License.

.PHONY: rebar distclean-rebar

# Configuration.

REBAR_CONFIG ?= $(CURDIR)/rebar.config

REBAR ?= $(CURDIR)/rebar
export REBAR

REBAR_URL ?= https://github.com/rebar/rebar/releases/download/2.5.1/rebar
REBAR_OPTS ?=

# Verbosity.

rebar_args_3 = -v 3
rebar_args_2 = -v 2
rebar_args_1 = -v 1
rebar_args = $(rebar_args_$(V))

rebar_verbose_0 = @echo " REBAR " $(@F);
rebar_verbose = $(rebar_verbose_$(V))

rebar = $(rebar_verbose) V=$(V) $(REBAR) $(rebar_args)

# Core targets.

distclean:: distclean-rebar

# Plugin-specific targets.

define rebar_fetch
	$(call core_http_get,$(REBAR),$(REBAR_URL))
	chmod +x $(REBAR)
endef

$(REBAR):
	@$(call rebar_fetch)

distclean-rebar:
	$(gen_verbose) rm -rf $(REBAR)

rebar:: $(REBAR)

# Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
# This file is part of erlang.mk and subject to the terms of the ISC License.

.PHONY: relx-rel distclean-relx-rel distclean-relx

# Configuration.

RELX_CONFIG ?= $(CURDIR)/relx.config

RELX ?= $(CURDIR)/relx
export RELX

RELX_URL ?= https://github.com/erlware/relx/releases/download/v1.0.4/relx
RELX_OPTS ?=
RELX_OUTPUT_DIR ?= _rel

ifeq ($(firstword $(RELX_OPTS)),-o)
	RELX_OUTPUT_DIR = $(word 2,$(RELX_OPTS))
else
	RELX_OPTS += -o $(RELX_OUTPUT_DIR)
endif

# Core targets.

ifneq ($(wildcard $(RELX_CONFIG)),)
rel:: distclean-relx-rel relx-rel
endif

distclean:: distclean-relx-rel distclean-relx

# Plugin-specific targets.

define relx_fetch
	$(call core_http_get,$(RELX),$(RELX_URL))
	chmod +x $(RELX)
endef

$(RELX):
	@$(call relx_fetch)

relx-rel: $(RELX)
	@$(RELX) -c $(RELX_CONFIG) $(RELX_OPTS)

distclean-relx-rel:
	$(gen_verbose) rm -rf $(RELX_OUTPUT_DIR)

distclean-relx:
	$(gen_verbose) rm -rf $(RELX)
