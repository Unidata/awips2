""" This is taken from the scrolled window example from the demo.

    Take a look at the DoDrawing2() method below.  The first 6 lines
    or so have been translated into C++.

"""
<<<<<<< HEAD

=======
from __future__ import absolute_import, print_function
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

import sys
sys.path.insert(0,'..')
import inline_tools

from wxPython.wx import *

<<<<<<< HEAD
class MyCanvas(wxScrolledWindow):
    def __init__(self, parent, id = -1, size = wxDefaultSize):
        wxScrolledWindow.__init__(self, parent, id, wxPoint(0, 0), size, wxSUNKEN_BORDER)

        self.lines = []
        self.maxWidth  = 1000
=======

class MyCanvas(wxScrolledWindow):
    def __init__(self, parent, id=-1, size=wxDefaultSize):
        wxScrolledWindow.__init__(self, parent, id, wxPoint(0, 0), size, wxSUNKEN_BORDER)

        self.lines = []
        self.maxWidth = 1000
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        self.maxHeight = 1000

        self.SetBackgroundColour(wxNamedColor("WHITE"))
        EVT_LEFT_DOWN(self, self.OnLeftButtonEvent)
<<<<<<< HEAD
        EVT_LEFT_UP(self,   self.OnLeftButtonEvent)
        EVT_MOTION(self,    self.OnLeftButtonEvent)

        EVT_PAINT(self, self.OnPaint)


        self.SetCursor(wxStockCursor(wxCURSOR_PENCIL))
        #bmp = images.getTest2Bitmap()
        #mask = wxMaskColour(bmp, wxBLUE)
        #bmp.SetMask(mask)
        #self.bmp = bmp
=======
        EVT_LEFT_UP(self, self.OnLeftButtonEvent)
        EVT_MOTION(self, self.OnLeftButtonEvent)

        EVT_PAINT(self, self.OnPaint)

        self.SetCursor(wxStockCursor(wxCURSOR_PENCIL))
        # bmp = images.getTest2Bitmap()
        # mask = wxMaskColour(bmp, wxBLUE)
        # bmp.SetMask(mask)
        # self.bmp = bmp
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        self.SetScrollbars(20, 20, self.maxWidth/20, self.maxHeight/20)

    def getWidth(self):
        return self.maxWidth

    def getHeight(self):
        return self.maxHeight

<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def OnPaint(self, event):
        dc = wxPaintDC(self)
        self.PrepareDC(dc)
        self.DoDrawing2(dc)

<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def DoDrawing(self, dc):
        dc.BeginDrawing()
        dc.SetPen(wxPen(wxNamedColour('RED')))
        dc.DrawRectangle(5, 5, 50, 50)

<<<<<<< HEAD
        dc.SetBrush(wxLIGHT_GREY_BRUSH)#
=======
        dc.SetBrush(wxLIGHT_GREY_BRUSH)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        dc.SetPen(wxPen(wxNamedColour('BLUE'), 4))
        dc.DrawRectangle(15, 15, 50, 50)

        dc.SetFont(wxFont(14, wxSWISS, wxNORMAL, wxNORMAL))
        dc.SetTextForeground(wxColour(0xFF, 0x20, 0xFF))
        te = dc.GetTextExtent("Hello World")
        dc.DrawText("Hello World", 60, 65)

        dc.SetPen(wxPen(wxNamedColour('VIOLET'), 4))
        dc.DrawLine(5, 65+te[1], 60+te[0], 65+te[1])

        lst = [(100,110), (150,110), (150,160), (100,160)]
        dc.DrawLines(lst, -60)
        dc.SetPen(wxGREY_PEN)
        dc.DrawPolygon(lst, 75)
        dc.SetPen(wxGREEN_PEN)
        dc.DrawSpline(lst+[(100,100)])

<<<<<<< HEAD
        #dc.DrawBitmap(self.bmp, 200, 20, true)
        #dc.SetTextForeground(wxColour(0, 0xFF, 0x80))
        #dc.DrawText("a bitmap", 200, 85)
=======
        # dc.DrawBitmap(self.bmp, 200, 20, true)
        # dc.SetTextForeground(wxColour(0, 0xFF, 0x80))
        # dc.DrawText("a bitmap", 200, 85)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        font = wxFont(20, wxSWISS, wxNORMAL, wxNORMAL)
        dc.SetFont(font)
        dc.SetTextForeground(wxBLACK)
        for a in range(0, 360, 45):
            dc.DrawRotatedText("Rotated text...", 300, 300, a)

        dc.SetPen(wxTRANSPARENT_PEN)
        dc.SetBrush(wxBLUE_BRUSH)
        dc.DrawRectangle(50,500,50,50)
        dc.DrawRectangle(100,500,50,50)

        dc.SetPen(wxPen(wxNamedColour('RED')))
        dc.DrawEllipticArc(200, 500, 50, 75, 0, 90)

        self.DrawSavedLines(dc)
        dc.EndDrawing()

    def DoDrawing2(self, dc):

<<<<<<< HEAD
        red = wxNamedColour("RED");
        blue = wxNamedColour("BLUE");
        grey_brush = wxLIGHT_GREY_BRUSH;
=======
        red = wxNamedColour("RED")
        blue = wxNamedColour("BLUE")
        grey_brush = wxLIGHT_GREY_BRUSH
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        code = \
        """
        //#line 108 "wx_example.py"
        dc->BeginDrawing();
        dc->SetPen(wxPen(*red,4,wxSOLID));
        dc->DrawRectangle(5, 5, 50, 50);

        dc->SetBrush(*grey_brush);
        dc->SetPen(wxPen(*blue, 4,wxSOLID));
        dc->DrawRectangle(15, 15, 50, 50);
        """
        inline_tools.inline(code,['dc','red','blue','grey_brush'],verbose=2)

        dc.SetFont(wxFont(14, wxSWISS, wxNORMAL, wxNORMAL))
        dc.SetTextForeground(wxColour(0xFF, 0x20, 0xFF))
        te = dc.GetTextExtent("Hello World")
        dc.DrawText("Hello World", 60, 65)

        dc.SetPen(wxPen(wxNamedColour('VIOLET'), 4))
        dc.DrawLine(5, 65+te[1], 60+te[0], 65+te[1])

        lst = [(100,110), (150,110), (150,160), (100,160)]
        dc.DrawLines(lst, -60)
        dc.SetPen(wxGREY_PEN)
        dc.DrawPolygon(lst, 75)
        dc.SetPen(wxGREEN_PEN)
        dc.DrawSpline(lst+[(100,100)])

<<<<<<< HEAD
        #dc.DrawBitmap(self.bmp, 200, 20, true)
        #dc.SetTextForeground(wxColour(0, 0xFF, 0x80))
        #dc.DrawText("a bitmap", 200, 85)
=======
        # dc.DrawBitmap(self.bmp, 200, 20, true)
        # dc.SetTextForeground(wxColour(0, 0xFF, 0x80))
        # dc.DrawText("a bitmap", 200, 85)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

        font = wxFont(20, wxSWISS, wxNORMAL, wxNORMAL)
        dc.SetFont(font)
        dc.SetTextForeground(wxBLACK)
        for a in range(0, 360, 45):
            dc.DrawRotatedText("Rotated text...", 300, 300, a)

        dc.SetPen(wxTRANSPARENT_PEN)
        dc.SetBrush(wxBLUE_BRUSH)
        dc.DrawRectangle(50,500,50,50)
        dc.DrawRectangle(100,500,50,50)

        dc.SetPen(wxPen(wxNamedColour('RED')))
        dc.DrawEllipticArc(200, 500, 50, 75, 0, 90)

        self.DrawSavedLines(dc)
        dc.EndDrawing()

<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def DrawSavedLines(self, dc):
        dc.SetPen(wxPen(wxNamedColour('MEDIUM FOREST GREEN'), 4))
        for line in self.lines:
            for coords in line:
                apply(dc.DrawLine, coords)

<<<<<<< HEAD

=======
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    def SetXY(self, event):
        self.x, self.y = self.ConvertEventCoords(event)

    def ConvertEventCoords(self, event):
        xView, yView = self.GetViewStart()
        xDelta, yDelta = self.GetScrollPixelsPerUnit()
        return (event.GetX() + (xView * xDelta),
                event.GetY() + (yView * yDelta))

    def OnLeftButtonEvent(self, event):
        if event.LeftDown():
            self.SetXY(event)
            self.curLine = []
            self.CaptureMouse()

        elif event.Dragging():
            dc = wxClientDC(self)
            self.PrepareDC(dc)
            dc.BeginDrawing()
            dc.SetPen(wxPen(wxNamedColour('MEDIUM FOREST GREEN'), 4))
            coords = (self.x, self.y) + self.ConvertEventCoords(event)
            self.curLine.append(coords)
            apply(dc.DrawLine, coords)
            self.SetXY(event)
            dc.EndDrawing()

        elif event.LeftUp():
            self.lines.append(self.curLine)
            self.curLine = []
            self.ReleaseMouse()

#---------------------------------------------------------------------------
# This example isn't currently used.

<<<<<<< HEAD
class py_canvas(wx.wxWindow):
    def __init__(self, parent, id = -1, pos=wx.wxPyDefaultPosition,
                 size=wx.wxPyDefaultSize, **attr):
        wx.wxWindow.__init__(self, parent, id, pos,size)
        #wx.EVT_PAINT(self,self.on_paint)
=======

class py_canvas(wx.wxWindow):
    def __init__(self, parent, id=-1, pos=wx.wxPyDefaultPosition,
                 size=wx.wxPyDefaultSize, **attr):
        wx.wxWindow.__init__(self, parent, id, pos,size)
        # wx.EVT_PAINT(self,self.on_paint)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        background = wx.wxNamedColour('white')

        code = """
               self->SetBackgroundColour(*background);
               """
        inline_tools.inline(code,['self','background'],compiler='msvc')
#----------------------------------------------------------------------------

<<<<<<< HEAD
=======

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
class MyFrame(wxFrame):
    def __init__(self, parent, ID, title, pos=wxDefaultPosition,
                 size=wxDefaultSize, style=wxDEFAULT_FRAME_STYLE):
        wxFrame.__init__(self, parent, ID, title, pos, size, style)
<<<<<<< HEAD
        #panel = wxPanel(self, -1)
        self.GetSize()
        #button = wxButton(panel, 1003, "Close Me")
        #button.SetPosition(wxPoint(15, 15))
        #EVT_BUTTON(self, 1003, self.OnCloseMe)
        #EVT_CLOSE(self, self.OnCloseWindow)
        #canvas = py_canvas(self,-1)
        canvas = MyCanvas(self,-1)
        canvas.Show(true)

class MyApp(wxApp):
    def OnInit(self):
        win = MyFrame(NULL, -1, "This is a wxFrame", size=(350, 200),
                      style = wxDEFAULT_FRAME_STYLE)# |  wxFRAME_TOOL_WINDOW )
=======
        # panel = wxPanel(self, -1)
        self.GetSize()
        # button = wxButton(panel, 1003, "Close Me")
        # button.SetPosition(wxPoint(15, 15))
        # EVT_BUTTON(self, 1003, self.OnCloseMe)
        # EVT_CLOSE(self, self.OnCloseWindow)
        # canvas = py_canvas(self,-1)
        canvas = MyCanvas(self,-1)
        canvas.Show(true)


class MyApp(wxApp):
    def OnInit(self):
        win = MyFrame(NULL, -1, "This is a wxFrame", size=(350, 200),
                      style = wxDEFAULT_FRAME_STYLE)  # |  wxFRAME_TOOL_WINDOW )
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
        win.Show(true)
        return true

if __name__ == "__main__":
    app = MyApp(0)
    app.MainLoop()
