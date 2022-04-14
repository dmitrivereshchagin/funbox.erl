COMPOSE := docker-compose

exec = $(COMPOSE) exec $(1)

exec-workspace := $(call exec,workspace)
exec-redis     := $(call exec,redis)

REBAR3 := $(exec-workspace) rebar3

.PHONY: all
all: workspace-up shell

.PHONY: workspace-build
workspace-build: ; $(COMPOSE) build

.PHONY: workspace-up
workspace-up: ; $(COMPOSE) up --detach

.PHONY: workspace-down
workspace-down: ; $(COMPOSE) down

.PHONY: workspace w
workspace w: ; $(exec-workspace) sh

.PHONY: shell s
shell s: ; $(REBAR3) shell

.PHONY: dialyzer d
dialyzer d: ; $(REBAR3) dialyzer

.PHONY: test t
test t:
	$(REBAR3) eunit --cover
	$(REBAR3) ct --cover
	$(REBAR3) cover --verbose

.PHONY: redis-cli r
redis-cli r: ; $(exec-redis) redis-cli
