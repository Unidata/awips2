# Interface to Mathematica for plotting
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2006-11-23
#
# Note: I haven't bee using this for ages. Perhaps it doesn't work with
# current Mathematica versions any more.
#

import Scientific.N as Numeric
import os, string, tempfile

# Class representing a Mathematica process

class Mathematica:

    def __init__(self, progname = 'math'):
        self.progname = progname
        self.script = ''
        self.font = "Courier"
        self.textsize = 10

    def clear(self):
        self.script = ''

    def execute(self):
        scriptfile = tempfile.mktemp()
        file = open(scriptfile, 'w')
        file.write(self.script)
        file.write('Quit\n')
        file.close()
        #os.system('cat ' + scriptfile)
        #commandline = self.progname + (' -run \'<<"%s"\'' % scriptfile)
        commandline = self.progname + ' < ' + scriptfile + ' >/dev/null 2>&1'
        os.system(commandline)
        os.unlink(scriptfile)

    def command(self, command):
        self.script = self.script + command

    def backup(self, n):
        self.script = self.script[:-n]

    def defineVariable(self, name, array):
        self.command(name + ' = ')
        self.command(formatValue(array))
        self.command('\n')

    def setFont(self, font, size):
        self.font = font
        self.textsize = size

    def displayOptions(self):
        s = 'DefaultFont->{"' + self.font + '",' + \
            formatValue(self.textsize) + '}'
        return s

    def plot(self, data, display = 1):
        s = 'graph = Show['
        i = 0
        for dataset in data:
            s = s + 'ListPlot[' + formatValue(dataset) + \
                ', PlotJoined->True, DisplayFunction->Identity, ' + \
                'PlotRange->All, PlotStyle->' + self.dash[i] + ', ' + \
                self.displayOptions() + '], '
            i = (i + 1) % len(self.dash)
        if display:
            s = s + 'DisplayFunction->$DisplayFunction]\n'
        else:
            s = s + 'DisplayFunction->Identity]\n'
        self.command(s)

    dash = ['Dashing[{0.1,0.0}]', 'Dashing[{0.03, 0.015}]',
            'Dashing[{0.03, 0.03}]', 'Dashing[{0.015, 0.015}]',
            'Dashing[{0.03, 0.015, 0.015, 0.015}]',
            'Dashing[{0.015, 0.015}]', 'Dashing[{0.015, 0.03}]']

    def contourPlot(self, xaxis, yaxis, data, contours=None, display=1):
        s = 'graph = Show[ContourGraphics[' \
            + formatValue(Numeric.transpose(data)) \
            +  ', MeshRange->' \
            + formatValue([[xaxis[0], xaxis[-1]], [yaxis[0], yaxis[-1]]]) \
            + ', ContourShading->False'
        if contours is not None:
            s = s + ', Contours->' + formatValue(contours)
        s = s + ', ' + self.displayOptions() + '],'
        if display:
            s = s + 'DisplayFunction->$DisplayFunction]\n'
        else:
            s = s + 'DisplayFunction->Identity]\n'
        self.command(s)

    def surfacePlot(self, xaxis, yaxis, data, display=1):
        s = 'graph = Show[ListPlot3D[' \
            + formatValue(Numeric.transpose(data)) \
            +  ', MeshRange->' \
            + formatValue([[xaxis[0], xaxis[-1]], [yaxis[0], yaxis[-1]]])
        s = s + ', ' + self.displayOptions() + '],'
        if display:
            s = s + 'DisplayFunction->$DisplayFunction]\n'
        else:
            s = s + 'DisplayFunction->Identity]\n'
        self.command(s)

    def printGraph(self, filename):
        self.command('Display["' + filename + '", graph, "EPS"]\n')


# Format scalars, arrays, and nested lists for Mathematica

def formatValue(x):
    is_sequence = 1
    try:
        x[0]
    except:
        is_sequence = 0
    if is_sequence:
        s = '{'
        for e in x:
            s = s + formatValue(e) + ', '
        s = s[:-2] + '}'
    else:
        if type(x) == type(''):
            s = '"' + x + '"'
        elif type(x) == type(0):
            s = `x`
        elif type(x) == type(0.):
            s = string.replace(`x`, 'e','*10^')
        elif type(x) == type(0.j):
            s = '(' + string.replace(`x.real`, 'e','*10^') + \
                '+' + string.replace(`x.imag`, 'e','*10^') + 'I)'
        else:
            raise TypeError('Unknown type ' + `type(x)`)
    return s


# Simple plot functions

def _output(m, options):
    if options.has_key('file'):
        m.printGraph(options['file'])
        m.execute()
    else:
        filename = tempfile.mktemp('.eps')
        m.printGraph(filename)
        m.execute()
        if os.fork() == 0:
            commandline = 'gv ' + filename + ' >/dev/null 2>&1'
            os.system(commandline)
            os.unlink(filename)
            os._exit(0)

def plot(*data, **options):
    m = Mathematica()
    m.plot(data, display=0)
    _output(m, options)

def contourPlot(xaxis, yaxis, data, contours=None, **options):
    m = Mathematica()
    m.contourPlot(xaxis, yaxis, data, contours, display=0)
    _output(m, options)

def surfacePlot(xaxis, yaxis, data, **options):
    m = Mathematica()
    m.surfacePlot(xaxis, yaxis, data, display=0)
    _output(m, options)

def multiplePlots(data, **options):
    m = Mathematica()
    i = 0
    lines = []
    for line in data:
        columns = []
        for item in line:
            if type(item) != type(()): item = (item, )
            name = 'plot'+`i`
            m.command(name + ' = ')
            m.plot(item, 0)
            columns.append(name)
            i = i + 1
        lines.append(columns)
    m.command('\ngraph = Show[GraphicsArray[{')
    for line in lines:
        m.command('{')
        for item in line:
            m.command(item + ', ')
        m.backup(2)
        m.command('}, ')
    m.backup(2)
    m.command('}], Displayfunction->$DisplayFunction]\n')
    if options.has_key('file'):
        m.printGraph(options['file'])
    m.execute()

# These examples are all the documentation you will get!
    
if __name__ == '__main__':

    from Scientific.N import arange, sin, NewAxis

#    data1 = [4,6,5,3]
#    data2 = [4,6,5,3]
#    multiplePlots([[data1], [data2]])
#    plot([4,6,5,3], file = 'junk.ps')
#    plot([(3,6.8),(4,4.2),(5,0.5)], [4,6,5,3])

    x = arange(10)
    y = arange(15)
    data = x[:, NewAxis]*sin(y/2.)
    #contourPlot(x, y, data, arange(0.,10.,0.1))
    surfacePlot(x, y, data)
