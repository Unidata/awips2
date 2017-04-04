[Font Awesome](https://github.com/FortAwesome/Font-Awesome) as a [PolymerElements/iron-iconset-svg](https://github.com/PolymerElements/iron-iconset-svg) component.

# Set Up
````bash
bower install fontawesome-iconset
````
# Use

Link your custom component alongside with other Polymer dependencies
````html
<link rel="import" href="bower_components/fontawesome-iconset/fa-icons.html">
````
Use the iconset
````html
<iron-icon icon="fa:line-chart"></iron-icon>
````
# Update

To update to the latest version of FontAwesome, just install node modules and run "update"
````bash
npm install
node update
````
Soon there will be a production script to export only the icons to be used (like the original [font-awesome-polymer-icons-generator](https://github.com/philya/font-awesome-polymer-icons-generator) but in node).
