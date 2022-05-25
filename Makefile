COMPOSE := docker-compose

exec = $(COMPOSE) exec $(1)

exec-workspace := $(call exec,workspace)
exec-redis     := $(call exec,redis)

REBAR3 := $(exec-workspace) rebar3

.PHONY: all
all: workspace-up shell

.PHONY: workspace-build wb
workspace-build wb: ; $(COMPOSE) build

.PHONY: workspace-up wu
workspace-up wu: ; $(COMPOSE) up --detach

.PHONY: workspace-down wd
workspace-down wd: ; $(COMPOSE) down

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

.PHONY: redis-cli rc
redis-cli rc: ; $(exec-redis) redis-cli
