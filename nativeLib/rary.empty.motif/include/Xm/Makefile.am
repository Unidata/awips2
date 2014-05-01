#
# $Header: /cvsroot/lesstif/lesstif/include/Motif-2.1/Xm/Makefile.am,v 1.24 2004/08/28 19:23:25 dannybackx Exp $
#
MAINTAINERCLEANFILES = Makefile.in Xm.h Xm.h.tmp
CLEANFILES=	Xm.h Xm.h.tmp

#
# Install headers
xmdir=	$(includedir)/Xm

# Only in 1.2
xm_12private_h = \
	ArrowBGP.h ArrowBP.h \
	DisplayP.h DrawP.h DrawingA.h DrawingAP.h DrawnB.h DrawnBP.h \
	DropSMgr.h DropSMgrP.h DropTrans.h DropTransP.h \
	FileSBP.h \
	GadgetP.h \
	LabelGP.h LabelP.h ListP.h \
	MenuShellP.h \
	PanedWP.h PrimitiveP.h\
	ScaleP.h ScrolledWP.h \
	ToggleBGP.h ToggleBP.h TransltnsP.h \
	VendorSEP.h \
	XmAll.h XmP.h XmStrDefs.h

# In other versions above as well
xm_12base_h = \
	ArrowB.h ArrowBG.h AtomMgr.h \
	BaseClassP.h BulletinB.h BulletinBP.h \
	CacheP.h CascadeB.h CascadeBG.h CascadeBGP.h CascadeBP.h \
	Command.h CommandP.h CutPaste.h CutPasteP.h \
	DesktopP.h DialogS.h DialogSEP.h DialogSP.h Display.h \
	DragC.h DragCP.h DragDrop.h DragIcon.h DragIconP.h \
	DragOverS.h DragOverSP.h DrawingA.h DrawnB.h DrawnBP.h \
	DropSMgr.h DropSMgrP.h DropTrans.h DropTransP.h \
	ExtObjectP.h \
	FileSB.h Form.h FormP.h Frame.h FrameP.h \
	Label.h LabelG.h  List.h  \
	MainW.h MainWP.h MenuShell.h MenuUtilP.h MessageB.h MessageBP.h \
	ManagerP.h MwmUtil.h \
	PanedW.h Protocols.h ProtocolsP.h PushB.h PushBG.h PushBGP.h PushBP.h \
	RCUtilsP.h RepType.h RowColumn.h RowColumnP.h \
	SashP.h Scale.h Screen.h ScreenP.h ScrollBar.h ScrollBarP.h \
	ScrolledW.h SelectioB.h SelectioBP.h SeparatoG.h \
	SeparatoGP.h Separator.h SeparatorP.h ShellEP.h \
	TearOffBP.h TearOffP.h Text.h TextF.h TextFP.h TextFSelP.h TextInP.h \
	TextOutP.h TextP.h TextSelP.h TextStrSoP.h ToggleB.h ToggleBG.h \
	VaSimpleP.h VendorS.h VendorSP.h VirtKeys.h VirtKeysP.h \
	WorldP.h \
	XmosP.h

# Only in 2.0
xm_20private_h =\
	CSText.h CSTextP.h \
	TransltnsP.h \
	XmAll.h XmStrDefs.h

# In other versions above as well
xm_20base_h = \
        AccTextT.h ActivatableT.h ArrowBGP.h ArrowBP.h \
	CareVisualT.h ColorObjP.h ComboBox.h ComboBoxP.h \
	Container.h ContainerP.h ContainerT.h ContItemT.h \
	DialogSavvyT.h DisplayP.h DrawP.h DrawingAP.h \
	FileSBP.h \
	Gadget.h GadgetP.h GrabShell.h GrabShellP.h \
	IconG.h IconGP.h IconH.h JoinSideT.h \
	LabelP.h LabelGP.h LayoutT.h ListP.h \
	Manager.h MenuShellP.h MenuT.h \
	NavigatorT.h Notebook.h NotebookP.h \
	PanedWP.h Primitive.h PrimitiveP.h \
	ScaleP.h ScrolledWP.h ScrollFrameT.h SpecRenderT.h SpinB.h SpinBP.h \
	TakesDefT.h ToggleBGP.h ToggleBP.h TraitP.h \
	Transfer.h TransferP.h TransferT.h TxtPropCv.h \
	XmP.h XpmP.h XmIm.h \
	VendorSEP.h

# Only in 2.1
xm_21private_h = \
        Print.h PrintSP.h \
	SSpinB.h SSpinBP.h \
	TransltnsP.h \
	XmAll.h XmStrDefs.h 

# There's no 2.2 yet :-)
xm_21base_h =


# EXTRA_DIST = Xm.h.in $(xm_21base_h) $(xm_21private_h)
EXTRA_DIST = Xm.h.in

xm_HEADERS = $(xm_12base_h) $(xm_20base_h) $(xm_21base_h) $(xm_21private_h)

xm_DATA = Xm.h

# To avoid useless re-building when dependencies are being used
# only touch Xm.h if it really needs to be updated.
# We create a new, temporary new Xm.h(.tmp) and compare it to the
# old existing one.
# So it will only trigger a rebuild if the version stamps have changed!
Xm.h: Xm.h.tmp
	cmp -s Xm.h.tmp Xm.h || rm -f Xm.h && cp Xm.h.tmp Xm.h

Xm.h.tmp: $(top_srcdir)/include/Motif-2.1/Xm/Xm.h.in Makefile
	sed -e "s/_MAJOR_VERSION_/$(MAJOR_VERSION)/" \
	    -e "s/_MINOR_VERSION_/$(MINOR_VERSION)/" \
	    -e "s/_PICO_VERSION_/$(PICO_VERSION)/"   \
	$(top_srcdir)/include/Motif-2.1/Xm/Xm.h.in >Xm.h.tmp
