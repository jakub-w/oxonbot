PYTHON_SOURCES := obclient.py discordbot.py
GUILE_SOURCES := oxonbot.scm oxonbot-protocol.scm sockserv.scm
SQLITE_DIR := $(CURDIR)/guile-sqlite3
SQLITE_FILE := $(SQLITE_DIR)/sqlite3.scm
OUTPUT_DIR := $(CURDIR)/output

GUILE_SITE := $$(guile -c "(display (%site-dir))")
GUILE_CCACHE := $$(guile -c "(display (%site-ccache-dir))")


all: oxonbot python discord

oxonbot: sqlite3 $(OUTPUT_DIR)
	cp $(GUILE_SOURCES) $(OUTPUT_DIR)
# cd $(OUTPUT_DIR) && \
# printf "%s\n" $(GUILE_SOURCES) | \
# 	xargs -I '{}' guild compile {} -o {}.go

python: obclient.py $(OUTPUT_DIR)
	cp obclient.py $(OUTPUT_DIR)

discord: discordbot.py python $(OUTPUT_DIR)
	cp discordbot.py $(OUTPUT_DIR)

sqlite3: $(OUTPUT_DIR) $(SQLITE_FILE)
	cp $(SQLITE_FILE) $(OUTPUT_DIR)

$(SQLITE_FILE):
	git submodule update --init --recursive
	cd $(SQLITE_DIR) && \
	autoreconf -vi && ./configure && make

$(OUTPUT_DIR):
	mkdir $(OUTPUT_DIR)

clean:
	rm -r $(OUTPUT_DIR)

# install:
# 	install --mode=644 \
# 		-D --target-directory=$(GUILE_SITE) \
# 		$(OUTPUT_DIR)/*.scm
# 	install --mode=644 \
# 		-D --target-directory=$(GUILE_CCACHE) \
# 		$(OUTPUT_DIR)/*.go

.PHONY: oxonbot python discord sqlite3 clean
