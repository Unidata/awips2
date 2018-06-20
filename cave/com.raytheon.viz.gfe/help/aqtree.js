/*
aqtree.js

Converts an unordered list to an explorer-style tree

To make this work, simply add one line to your HTML:
<script type="text/javascript" src="aqtree.js"></script>

and then make the top UL of your nested unordered list of class "aqtree".

That's it. No registration function, nothing.

http://www.kryogenix.org/code/browser/aqlists/

Stuart Langridge, November 2002
sil@kryogenix.org

Inspired by Aaron's labels.js (http://youngpup.net/demos/labels/) and Dave Lindquist's menuDropDown.js (http://www.gazingus.org/dhtml/?id=109)

and now heavily hacked by Shariin of Fennin Ro  =)

With revisions by Phil Kulikauskas

*/


addEvent(window, "load", makeTrees);

function makeTrees () {
	// We don't actually need createElement, but we do
	// need good DOM support, so this is a good check.
	if (!document.createElement) return;
   
	uls = document.getElementsByTagName ("ul");
	for (uli=0; uli < uls.length; uli++)
	{
		ul = uls[uli];
		if (ul.nodeName == "UL" && ul.className == "aqtree")
		{
			processULEL (ul);
		}
	}
}

function processULEL (ul)
{
	if (!ul.childNodes || ul.childNodes.length == 0) return;

	var log = document.getElementById ("log");

	// Iterate LIs
	for (var itemi = 0; itemi < ul.childNodes.length; itemi++)
	{
		window.status = "item " + itemi;
		var item = ul.childNodes[itemi];
		if (item.nodeName == "LI")
		{
			item.style.listStyle = "none";

			// Iterate things in this LI
			var atag;
			var subul = 0;

			// rather than just playing with the a tag, we
			// want to insert a new a tag and play with *that*

			// create the a tag
			atag = document.createElement ("a");
			atag.setAttribute ("class", "treenode");
			// put a '-' inside it, as we're defaulting to an open tree
			// we could instead put a + in it and set the ul to display:none
			// to default to a closed tree
			icon = document.createElement ("img");	// AQ5
			if (item.className == "expand")
			{
				icon.setAttribute ("src", "../images/bullet.gif");
			}
			else
			{
				icon.setAttribute ("src", "../images/plus.gif");	// AQ5
			}
			atag.appendChild (icon);	// AQ5

			// insert it before our link
			var first = item.firstChild;
			item.insertBefore (atag, first);
			// and insert a space to look pretty
			item.insertBefore (document.createTextNode (" "), first);

			for (var sitemi = 0; sitemi < item.childNodes.length; sitemi++)
			{
				if (item.childNodes[sitemi].nodeName == "UL")
				{
					subul = item.childNodes[sitemi];
					// conceal the contents of this child, so we default
					// to a closed tree
					if (item.className != "expand")
					{
						subul.style.display = "none";	// AQ5
					}
					processULEL (subul);
				}
			}
			if (subul && item.className != "expand") {
				// this node has children, tie in the open/close handlers
				associateEL (atag, subul);
			} else {
				// this node has no children; just display a bullet
				icon = document.createElement ("img");	// AQ5
				icon.setAttribute ("src", "../images/bullet.gif");	// AQ5
				atag.parentNode.replaceChild (icon, atag);	// AQ5
			}
		}
	}
}

function associateEL (anchor, ul)
{
	if (!anchor) alert ("associateEL");

	anchor.onclick = function ()
	{
		var display = ul.style.display;
		if (ul.style.display == "none")
		{
			// open the node
			ul.style.display = "block";
			// change the + to a -
			icon = document.createElement ("img");	// AQ5
			icon.setAttribute ("src", "../images/minus.gif");	// AQ5
			anchor.replaceChild (icon, anchor.firstChild);	// AQ5
		}
		else
		{
			// close the node
			ul.style.display = "none";
			// change the - to a +
			icon = document.createElement ("img");	// AQ5
			icon.setAttribute ("src", "../images/plus.gif");	// AQ5
			anchor.replaceChild (icon, anchor.firstChild);	// AQ5
		}
		return false;
	}
	anchor.onmouseover = function()
	{
		var display = ul.style.display;
		window.status = (display == "block") ? "Collapse" : "Expand";
		return true;
	}
	anchor.onmouseout = function()
	{
		window.status = "";
		return true;
	}
}

/*			  Utility functions					*/

function addEvent(obj, evType, fn){
  /* adds an eventListener for browsers which support it
	 Written by Scott Andrew: nice one, Scott */
  if (obj.addEventListener){
	obj.addEventListener(evType, fn, true);
	return true;
  } else if (obj.attachEvent){
   var r = obj.attachEvent("on"+evType, fn);
	return r;
  } else {
   return false;
  }
}
