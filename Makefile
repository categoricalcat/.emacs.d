PERSONAL := prelude/personal

.PHONY: install link unlink clean start stop restart

install: submodule link ## Full bootstrap
	@echo "[femacs] install complete"

submodule: ## Pull prelude submodule
	git submodule update --init --recursive

link: ## Link prelude-modules.el into prelude/personal
	@mkdir -p $(PERSONAL)
	ln -sfn ../../prelude-modules.el $(PERSONAL)/prelude-modules.el
	@echo "[femacs] linked"

unlink: ## Remove managed symlinks
	rm -f $(PERSONAL)/prelude-modules.el

clean: unlink ## Nuke generated dirs (elpa, eln-cache, etc.)
	rm -rf elpa eln-cache auto-save-list savefile desktop cache

start: ## Start daemon
	emacs --daemon --debug-init

stop: ## Stop daemon
	emacsclient --eval '(kill-emacs)' 2>/dev/null || true

restart: stop start ## Restart daemon

help: ## Show targets
	@grep -E '^[a-z_-]+:.*##' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*## "}; {printf "  %-12s %s\n", $$1, $$2}'
