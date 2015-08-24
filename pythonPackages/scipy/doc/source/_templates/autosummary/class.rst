{% extends "!autosummary/class.rst" %}

{% block methods %}
{% if methods %}
<<<<<<< HEAD
   .. HACK
      .. autosummary::
         :toctree:
      {% for item in methods %}
         {{ name }}.{{ item }}
=======
   .. HACK -- the point here is that we don't want this to appear in the output, but the autosummary should still generate the pages.
      .. autosummary::
         :toctree:
      {% for item in all_methods %}
         {%- if not item.startswith('_') or item in ['__call__'] %}
         {{ name }}.{{ item }}
         {%- endif -%}
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      {%- endfor %}
{% endif %}
{% endblock %}

{% block attributes %}
{% if attributes %}
<<<<<<< HEAD
   .. HACK
      .. autosummary::
         :toctree:
      {% for item in attributes %}
         {{ name }}.{{ item }}
=======
   .. HACK -- the point here is that we don't want this to appear in the output, but the autosummary should still generate the pages.
      .. autosummary::
         :toctree:
      {% for item in all_attributes %}
         {%- if not item.startswith('_') %}
         {{ name }}.{{ item }}
         {%- endif -%}
>>>>>>> 85b42d3bbdcef5cbe0fe2390bba8b3ff1608040b
      {%- endfor %}
{% endif %}
{% endblock %}
