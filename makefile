test:
	emacs --batch \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\"))" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(dolist (pkg '(request cl-lib transient auth-source json org)) (unless (package-installed-p pkg) (package-install pkg)))" \
		-l ert \
		-L . \
		-l elm.test.el \
		-f ert-run-tests-batch-and-exit
