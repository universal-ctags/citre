# See https://github.com/universal-ctags/ctags/issues/2890
CTAGS=${1:-ctags}
${CTAGS} -o target.tags \
	 --kinds-all='*' --fields='*'-T --extras='*' \
	 --pseudo-tags=-TAG_PROGRAM_VERSION \
	 --pseudo-tags=-TAG_PROC_CWD \
	 -R src
