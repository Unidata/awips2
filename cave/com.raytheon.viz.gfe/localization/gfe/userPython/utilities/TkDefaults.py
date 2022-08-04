##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Jan 23, 2018  7153     randerso  Changes to allow new GFE config file to be 
#                                  selected when perspective is re-opened.
#
##

##
# This is an absolute override file, indicating that a higher priority version
# of the file will completely replace a lower priority version of the file.
##

from com.raytheon.viz.gfe import GFEPreference

Options = [
#            ('*visual',                             'truecolor'),
            ('*background' ,                        'gray65'),
            ('*activeBackground' ,                  'gray83'),
            ('*blinkingHighlightColor' ,            'CornSilk'),
            ('*disabledForeground' ,                'gray48'),
            ('*displacedTimeBackground' ,           'black'),
            ('*displacedTimeForeground' ,           'yellow'),
            ('*foreground' ,                        'black'),
            ('*highlightBackground' ,               'gray65'),
            ('*lightBackground' ,                   'gray90'),
            ('*productAvailableForeground' ,        'springgreen1'),
            ('*productNotAvailableForeground' ,     'black'),
            ('*selectColor' ,                       'yellow'),
            ('*shadowBackground' ,                  '#818181'),
            ('*titleColor' ,                        'blue'),
            ('*troughColor' ,                       'gray58'),
            ('*urgentActiveBackground' ,            'red'),
            ('*urgentBackground' ,                  'red2'),
            ('*urgentMsgFg' ,                       'red2'),
            ('*urgentMsgBg' ,                       'white'),
            ('*urgentMsgG' ,                        'gray40'),
            ('*significantMsgFg' ,                  'yellow'),
            ('*significantMsgBg' ,                  'black'),
            ('*significantMsgG' ,                   'Gray80'),
            ('*regularMsgFg' ,                      'green'),
            ('*regularMsgBg' ,                      'gray40'),
            ('*regularMsgG' ,                       'gray80'),
            ('*Checkbutton.selectColor' ,           'yellow'),
            ('*Entry.background' ,                  'WhiteSmoke'),
            ('*Entry.foreground' ,                  'Black'),
            ('*Entry.selectBackground' ,            'CornSilk'),
            ('*Entry.selectForeground' ,            'Black'),
            ('*Listbox.background' ,                'WhiteSmoke'),
            ('*Listbox.foreground' ,                'Black'),
            ('*Listbox.selectBackground' ,          'CornSilk'),
            ('*Listbox.selectForeground' ,          'Black'),
            ('*Listbox.exportSelection' ,           'no'),
            ('*Menu.selectColor' ,                  'yellow'),
            ('*Radiobutton.selectColor' ,           'yellow'),
            ('*Text.background' ,                   'WhiteSmoke'),
            ('*Text.foreground' ,                   'Black'),
            ('*Text.selectBackground' ,             'CornSilk'),
            ('*Text.selectForeground' ,             'Black'),
            ('*menubarPadY' ,                       '2'),
            ('*Button.borderWidth' ,                '2'),
            ('*Button.highlightThickness' ,         '1'),
            ('*Button.padX' ,                       '7'),
            ('*Button.padY' ,                       '2'),
            ('*Canvas.highlightThickness' ,         '1'),
            ('*Canvas.font' ,         '-b&h-lucidatypewriter-bold-*-sans-14-*'),
            ('*Checkbutton.borderWidth' ,           '2'),
            ('*Checkbutton.highlightThickness' ,    '1'),
            ('*Entry.borderWidth' ,                 '2'),
            ('*Entry.highlightThickness' ,          '1'),
            ('*Listbox.borderWidth' ,               '2'),
            ('*Listbox.highlightThickness' ,        '1'),
            ('*Listbox.selectBorderWidth' ,         '1'),
            ('*Menu.borderWidth' ,                  '1'),
            ('*Listbox.selectBorderWidth' ,         '1'),
            ('*Menu.borderWidth' ,                  '1'),
            ('*Menubutton.highlightThickness' ,     '1'),
            ('*Radiobutton.borderWidth' ,           '2'),
            ('*Radiobutton.highlightThickness' ,    '1'),
            ('*Scale.borderWidth' ,                 '2'),
            ('*Scale.highlightThickness' ,          '1'),
            ('*Scrollbar.borderWidth' ,             '2'),
            ('*Scrollbar.highlightThickness' ,      '1'),
            ('*Text.borderWidth' ,                  '2'),
            ('*Text.highlightThickness' ,           '1'),
            ('*Text.font',             '-*-fixed-*-*-*-*-*-240-*-*-*-*-*-*'),
            ('*Listbox.highlightThickness' ,        '1'),
            ('*fixedFont', "-b&h-helvetica-medium-*-*-14-*"),
            ('*font', "-b&h-helvetica-medium-*-*-14-*"),
            ('*italicFont', "-b&h-helvetica-medium-*-*-14-*"),
            ('*statusFont', "-b&h-helvetica-medium-*-*-14-*"),
            ('*urgentFont', "-b&h-helvetica-medium-*-*-14-*")
            ]


def setDefaults(w):
      tk_font = GFEPreference.getString("Tk_font", "-b&h-lucidatypewriter-bold-*-sans-14-*")

      fontList = ['*fixedFont', '*font', '*italicFont', '*statusFont',
                  '*urgentFont']
      for font in fontList:
          Options.append((font, tk_font))
          
      for option in Options:
          #print option[0], option[1]
          w.option_add(option[0], option[1])
          
