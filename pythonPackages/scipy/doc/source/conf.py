# -*- coding: utf-8 -*-

import sys, os, re

<<<<<<< HEAD
# If your extensions are in another directory, add it here. If the directory
# is relative to the documentation root, use os.path.abspath to make it
# absolute, like shown here.
sys.path.insert(0, os.path.abspath('../sphinxext'))

# Check Sphinx version
import sphinx
if sphinx.__version__ < "0.5":
    raise RuntimeError("Sphinx 0.5.dev or newer required")

=======
# Check Sphinx version
import sphinx
if sphinx.__version__ < "1.1":
    raise RuntimeError("Sphinx 1.1 or newer required")

needs_sphinx = '1.1'
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

# -----------------------------------------------------------------------------
# General configuration
# -----------------------------------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be extensions
# coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
<<<<<<< HEAD
extensions = ['sphinx.ext.autodoc', 'sphinx.ext.pngmath', 'numpydoc',
              'sphinx.ext.intersphinx', 'sphinx.ext.coverage', 'plot_directive']

if sphinx.__version__ >= "0.7":
    extensions.append('sphinx.ext.autosummary')
else:
    extensions.append('autosummary')
    extensions.append('only_directives')

=======

sys.path.insert(0, os.path.abspath('../sphinxext'))
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)))

extensions = ['sphinx.ext.autodoc', 'sphinx.ext.mathjax', 'numpydoc',
              'sphinx.ext.intersphinx', 'sphinx.ext.coverage',
              'sphinx.ext.autosummary', 'scipyoptdoc']

# Determine if the matplotlib has a recent enough version of the
# plot_directive.
try:
    from matplotlib.sphinxext import plot_directive
except ImportError:
    use_matplotlib_plot_directive = False
else:
    try:
        use_matplotlib_plot_directive = (plot_directive.__version__ >= 2)
    except AttributeError:
        use_matplotlib_plot_directive = False

if use_matplotlib_plot_directive:
    extensions.append('matplotlib.sphinxext.plot_directive')
else:
    raise RuntimeError("You need a recent enough version of matplotlib")
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# General substitutions.
project = 'SciPy'
<<<<<<< HEAD
copyright = '2008-2009, The Scipy community'

# The default replacements for |version| and |release|, also used in various
# other places throughout the built documents.
#
import scipy
# The short X.Y version (including the .devXXXX suffix if present)
version = re.sub(r'^(\d+\.\d+)\.\d+(.*)', r'\1\2', scipy.__version__)
if 'dev' in version:
    # retain the .dev suffix, but clean it up
    version = re.sub(r'(\.dev\d*).*?$', r'\1', version)
else:
    # strip all other suffixes
    version = re.sub(r'^(\d+\.\d+).*?$', r'\1', version)
# The full version, including alpha/beta/rc tags.
release = scipy.__version__

print "Scipy (VERSION %s) (RELEASE %s)" % (version, release)
=======
copyright = '2008-2014, The Scipy community'

# The default replacements for |version| and |release|, also used in various
# other places throughout the built documents.
import scipy
version = re.sub(r'\.dev-.*$', r'.dev', scipy.__version__)
release = scipy.__version__

print "Scipy (VERSION %s)" % (version,)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
#today = ''
# Else, today_fmt is used as the format for a strftime call.
today_fmt = '%B %d, %Y'

# List of documents that shouldn't be included in the build.
#unused_docs = []

# The reST default role (used for this markup: `text`) to use for all documents.
default_role = "autolink"

# List of directories, relative to source directories, that shouldn't be searched
# for source files.
exclude_dirs = []

# If true, '()' will be appended to :func: etc. cross-reference text.
add_function_parentheses = False

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
#add_module_names = True

# If true, sectionauthor and moduleauthor directives will be shown in the
# output. They are ignored by default.
show_authors = False

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'


# -----------------------------------------------------------------------------
# HTML output
# -----------------------------------------------------------------------------

<<<<<<< HEAD
# The style sheet to use for HTML and HTML Help pages. A file of that name
# must exist either in Sphinx' static/ path, or in one of the custom paths
# given in html_static_path.
html_style = 'scipy.css'

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
html_title = "%s v%s Reference Guide (DRAFT)" % (project, version)

# The name of an image file (within the static path) to place at the top of
# the sidebar.
html_logo = '_static/scipyshiny_small.png'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
html_last_updated_fmt = '%b %d, %Y'

# Correct index page
#html_index = "index"

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
#html_use_smartypants = True

# Custom sidebar templates, maps document names to template names.
html_sidebars = {
    'index': 'indexsidebar.html'
}

# Additional templates that should be rendered to pages, maps page names to
# template names.
html_additional_pages = {}

# If false, no module index is generated.
html_use_modindex = True

# If true, the reST sources are included in the HTML build as _sources/<name>.
#html_copy_source = True

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
#html_use_opensearch = ''

# If nonempty, this is the file name suffix for HTML files (e.g. ".html").
html_file_suffix = '.html'

# Output file base name for HTML help builder.
htmlhelp_basename = 'scipy'

# Pngmath should try to align formulas properly
pngmath_use_preview = True
=======
themedir = os.path.join(os.pardir, 'scipy-sphinx-theme', '_theme')
if os.path.isdir(themedir):
    html_theme = 'scipy'
    html_theme_path = [themedir]

    if 'scipyorg' in tags:
        # Build for the scipy.org website
        html_theme_options = {
            "edit_link": True,
            "sidebar": "right",
            "scipy_org_logo": True,
            "rootlinks": [("http://scipy.org/", "Scipy.org"),
                          ("http://docs.scipy.org/", "Docs")]
        }
    else:
        # Default build
        html_theme_options = {
            "edit_link": False,
            "sidebar": "left",
            "scipy_org_logo": False,
            "rootlinks": []
        }
        html_logo = '_static/scipyshiny_small.png'
        html_sidebars = {'index': 'indexsidebar.html'}
else:
    # Build without scipy.org sphinx theme present
    if 'scipyorg' in tags:
        raise RuntimeError("Get the scipy-sphinx-theme first, "
                           "via git submodule init & update")
    else:
        html_style = 'scipy_fallback.css'
        html_logo = '_static/scipyshiny_small.png'
        html_sidebars = {'index': 'indexsidebar.html'}

html_title = "%s v%s Reference Guide" % (project, version)
html_static_path = ['_static']
html_last_updated_fmt = '%b %d, %Y'

html_additional_pages = {}
html_use_modindex = True
html_copy_source = False
html_file_suffix = '.html'

htmlhelp_basename = 'scipy'

pngmath_use_preview = True
pngmath_dvipng_args = ['-gamma', '1.5', '-D', '96', '-bg', 'Transparent']
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b


# -----------------------------------------------------------------------------
# LaTeX output
# -----------------------------------------------------------------------------

# The paper size ('letter' or 'a4').
#latex_paper_size = 'letter'

# The font size ('10pt', '11pt' or '12pt').
#latex_font_size = '10pt'

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, document class [howto/manual]).
_stdauthor = 'Written by the SciPy community'
latex_documents = [
  ('index', 'scipy-ref.tex', 'SciPy Reference Guide', _stdauthor, 'manual'),
#  ('user/index', 'scipy-user.tex', 'SciPy User Guide',
#   _stdauthor, 'manual'),
]

# The name of an image file (relative to this directory) to place at the top of
# the title page.
#latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
#latex_use_parts = False

# Additional stuff for the LaTeX preamble.
latex_preamble = r'''
\usepackage{amsmath}
<<<<<<< HEAD
\DeclareUnicodeCharacter{00A0}{\nobreakspace}

% In the parameters section, place a newline after the Parameters
% header
\usepackage{expdlist}
\let\latexdescription=\description
\def\description{\latexdescription{}{} \breaklabel}

% Make Examples/etc section headers smaller and more compact
\makeatletter
\titleformat{\paragraph}{\normalsize\py@HeaderFamily}%
            {\py@TitleColor}{0em}{\py@TitleColor}{\py@NormalColor}
\titlespacing*{\paragraph}{0pt}{1ex}{0pt}
\makeatother

=======

\DeclareUnicodeCharacter{00A0}{\nobreakspace}

% In the parameters etc. sections, align uniformly, and adjust label emphasis
\usepackage{expdlist}
\let\latexdescription=\description
\let\endlatexdescription=\enddescription
\renewenvironment{description}%
{\begin{latexdescription}[\setleftmargin{60pt}\breaklabel\setlabelstyle{\bfseries\itshape}]}%
{\end{latexdescription}}

% Make Examples/etc section headers smaller and more compact
\makeatletter
\titleformat{\paragraph}{\normalsize\normalfont\bfseries\itshape}%
            {\py@NormalColor}{0em}{\py@NormalColor}{\py@NormalColor}
\titlespacing*{\paragraph}{0pt}{1ex}{0pt}
\makeatother

% Save vertical space in parameter lists and elsewhere
\makeatletter
\renewenvironment{quote}%
               {\list{}{\topsep=0pt%
                        \parsep \z@ \@plus\p@}%
                \item\relax}%
               {\endlist}
\makeatother

>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
% Fix footer/header
\renewcommand{\chaptermark}[1]{\markboth{\MakeUppercase{\thechapter.\ #1}}{}}
\renewcommand{\sectionmark}[1]{\markright{\MakeUppercase{\thesection.\ #1}}}
'''

# Documents to append as an appendix to all manuals.
#latex_appendices = []

# If false, no module index is generated.
latex_use_modindex = False


# -----------------------------------------------------------------------------
# Intersphinx configuration
# -----------------------------------------------------------------------------
intersphinx_mapping = {
        'http://docs.python.org/dev': None,
        'http://docs.scipy.org/doc/numpy': None,
}


# -----------------------------------------------------------------------------
# Numpy extensions
# -----------------------------------------------------------------------------

# If we want to do a phantom import from an XML file for all autodocs
phantom_import_file = 'dump.xml'

<<<<<<< HEAD
# Edit links
#numpydoc_edit_link = '`Edit </pydocweb/doc/%(full_name)s/>`__'
=======
# Generate plots for example sections
numpydoc_use_plots = True
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

# -----------------------------------------------------------------------------
# Autosummary
# -----------------------------------------------------------------------------

<<<<<<< HEAD
if sphinx.__version__ >= "0.7": 
=======
if sphinx.__version__ >= "0.7":
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    import glob
    autosummary_generate = glob.glob("*.rst")

# -----------------------------------------------------------------------------
# Coverage checker
# -----------------------------------------------------------------------------
coverage_ignore_modules = r"""
    """.split()
coverage_ignore_functions = r"""
    test($|_) (some|all)true bitwise_not cumproduct pkgload
    generic\.
    """.split()
coverage_ignore_classes = r"""
    """.split()

coverage_c_path = []
coverage_c_regexes = {}
coverage_ignore_c_items = {}


#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------
plot_pre_code = """
import numpy as np
<<<<<<< HEAD
import scipy as sp
np.random.seed(123)
"""
plot_include_source = True
plot_formats = [('png', 100), 'pdf']
=======
np.random.seed(123)
"""
plot_include_source = True
plot_formats = [('png', 96), 'pdf']
plot_html_show_formats = False
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b

import math
phi = (math.sqrt(5) + 1)/2

<<<<<<< HEAD
import matplotlib
matplotlib.rcParams.update({
    'font.size': 8,
    'axes.titlesize': 8,
    'axes.labelsize': 8,
    'xtick.labelsize': 8,
    'ytick.labelsize': 8,
    'legend.fontsize': 8,
=======
font_size = 13*72/96.0  # 13 px

plot_rcparams = {
    'font.size': font_size,
    'axes.titlesize': font_size,
    'axes.labelsize': font_size,
    'xtick.labelsize': font_size,
    'ytick.labelsize': font_size,
    'legend.fontsize': font_size,
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
    'figure.figsize': (3*phi, 3),
    'figure.subplot.bottom': 0.2,
    'figure.subplot.left': 0.2,
    'figure.subplot.right': 0.9,
    'figure.subplot.top': 0.85,
    'figure.subplot.wspace': 0.4,
    'text.usetex': False,
<<<<<<< HEAD
})
=======
}

if not use_matplotlib_plot_directive:
    import matplotlib
    matplotlib.rcParams.update(plot_rcparams)

# -----------------------------------------------------------------------------
# Source code links
# -----------------------------------------------------------------------------

import inspect
from os.path import relpath, dirname

for name in ['sphinx.ext.linkcode', 'linkcode', 'numpydoc.linkcode']:
    try:
        __import__(name)
        extensions.append(name)
        break
    except ImportError:
        pass
else:
    print "NOTE: linkcode extension not found -- no links to source generated"

def linkcode_resolve(domain, info):
    """
    Determine the URL corresponding to Python object
    """
    if domain != 'py':
        return None

    modname = info['module']
    fullname = info['fullname']

    submod = sys.modules.get(modname)
    if submod is None:
        return None

    obj = submod
    for part in fullname.split('.'):
        try:
            obj = getattr(obj, part)
        except:
            return None

    try:
        fn = inspect.getsourcefile(obj)
    except:
        fn = None
    if not fn:
        try:
            fn = inspect.getsourcefile(sys.modules[obj.__module__])
        except:
            fn = None
    if not fn:
        return None

    try:
        source, lineno = inspect.getsourcelines(obj)
    except:
        lineno = None

    if lineno:
        linespec = "#L%d-L%d" % (lineno, lineno + len(source) - 1)
    else:
        linespec = ""

    fn = relpath(fn, start=dirname(scipy.__file__))

    if 'dev' in scipy.__version__:
        return "http://github.com/scipy/scipy/blob/master/scipy/%s%s" % (
           fn, linespec)
    else:
        return "http://github.com/scipy/scipy/blob/v%s/scipy/%s%s" % (
           scipy.__version__, fn, linespec)
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
