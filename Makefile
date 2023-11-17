# @Copyright None
# @Author Matthieu GOSSET
# @Maintainers Matthieu GOSSET <matthieu.gosset.dev@chapsvision.com>
# @Purpose make targets

DUNE=dune
CC=$(DUNE)
BUILD=$(CC) build
EXEC=$(CC) exec --profile=release
RUN=_build/default/

PATH_DEMO=bin/demo
DEMO_BIN=demo.exe

PATH_CLIENT=bin/client-server
CLIENT_BIN=client.exe
PATH_SERVER=bin/client-server
SERVER_BIN=server.exe

.PHONY: all demo client-server

all:
	$(BUILD)

$(PATH_CLIENT)/$(CLIENT_BIN):
	$(BUILD) $@

$(PATH_SERVER)/$(SERVER_BIN):
	$(BUILD) $@

client-server: $(PATH_CLIENT)/$(CLIENT_BIN) $(PATH_SERVER)/$(SERVER_BIN)
	$(RUN)/$(PATH_SERVER)/$(SERVER_BIN) 3001 3000 & \
	sleep 2 && \
	$(RUN)/$(PATH_CLIENT)/$(CLIENT_BIN) 3000 3001

$(PATH_DEMO)/$(DEMO_BIN):
	$(BUILD) $@

demo: $(PATH_DEMO)/$(DEMO_BIN)
	$(EXEC) $< 3000

help:
	cat README.md