TOP=..
include $(TOP)/mk/boilerplate.mk

PROG=runstdtest
CLEAN_FILES += $(PROG)
all::
	$(RM) -f $(PROG)
	echo '#!$(PERL)'                               >> $(PROG)
	echo '$$RM             = "$(RM)";'             >> $(PROG)
	echo '$$DEFAULT_TMPDIR = "$(DEFAULT_TMPDIR)";' >> $(PROG)
	echo '$$CONTEXT_DIFF   = "$(CONTEXT_DIFF)";'   >> $(PROG)
	cat $(PROG).prl                                >> $(PROG)
	$(EXECUTABLE_FILE) $(PROG)

boot :: all

include $(TOP)/mk/ghc-target.mk
