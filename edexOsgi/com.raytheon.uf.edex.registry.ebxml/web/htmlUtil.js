/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
 
 /**
 * 
 * File containing utility functions used by the registry web interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 8/8/2012     #724       bphillip     Initial creation
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
 
/*
 *  Gets the user ID from the URL
 */
function getUserId(){
    var url = window.location.href;
    var qparts = url.split("?");
    if(qparts.length>1){
        var tokens = qparts[1].split("&");
        for(var i = 0; i < tokens.length; i++){
            var subTokens=tokens[i].split("=")
            if(subTokens[0] == "id"){
                return subTokens[1];
            } 
        }
    }
    return null
}

/*
 *  Gets the mode from the URL
 */
function getMode(){
    var url = window.location.href;
    var qparts = url.split("?");
    if(qparts.length>1){
        var tokens = qparts[1].split("&");
        for(var i = 0; i < tokens.length; i++){ 
            var subTokens=tokens[i].split("=")
            if (subTokens[0] == "mode"){
                return subTokens[1];
            }
        }
    }
    return null
}


/*
 * Executes an HTTP POST request to the given path with the provided
 * parameters
 */
function submitPostRequest(path, params) {
    submitHTTPRequest(path,params, "post")
}

/*
 * Executes an HTTP GET request to the given path with the provided
 * parameters
 */
function submitGetRequest(path, params){
    submitHTTPRequest(path,params, "get") 
}

/*
 * Executes an HTTP request specified by the 'method' variable to the 
 * given path with the provided parameters
 */
function submitHTTPRequest(path, params, method){
    method = method || "post";

    var form = document.createElement("form");
    form.setAttribute("method", method);
    form.setAttribute("action", path);

    for(var key in params) {
        if(params.hasOwnProperty(key)) {
            var hiddenField = document.createElement("input");
            hiddenField.setAttribute("type", "hidden");
            hiddenField.setAttribute("name", key);
            hiddenField.setAttribute("value", params[key]);
            form.appendChild(hiddenField);
         }
    }
    document.body.appendChild(form);
    form.submit();
}

/*
 * Gets the value attribute of the given HTML element
 */
function getElementValue(elemName){
    var elem = document.getElementById(elemName)
    if(elem == null || elem.value==null){
        return null
    }else{
        return elem.value;
    }
}

/*
 * Sets the value attribute of the given HTML element
 */
function setElementValue(elemName,val){
    var elem = document.getElementById(elemName)
    
    if(elem != null){
        if(elemName.endsWith("Text")){
            elem.value = val;
        } else if (elemName.endsWith("Select")){
            setComboValue(elemName,val);
        } else if (elemName.endsWith("Span")){
            setSpanValue(elemName,val);
        }
    }
}

/*
 * Gets the text of the currently selected item in a select box
 */
function getComboValue(elemName){
    var elem = document.getElementById(elemName);
    var idx = elem.selectedIndex;
    if(idx < 0){
        return null
    }
    return elem.options[elem.selectedIndex].value
}

/*
 * Sets the currently selected index of a select box based on the 
 * given text string
 */
function setComboValue(elemName, val){
    var elem = document.getElementById(elemName);
    for(var i = 0; i < elem.options.length;i++){
        if(val == elem.options[i].value){
            elem.selectedIndex = i;
        }
    }
}

/*
 * Sets the value of an HTML span
 */
function setSpanValue(elemName, val){
    var elem = document.getElementById(elemName)
    elem.innerHTML = val;
}

/*
 * Gets the value of an HTML span
 */
function getSpanValue(elemName,val){
    var elem = document.getElementById(elemName);
    return elem.innerHTML;
}

/*
 * Sets the read only attribute of an HTTP element with the given name 
 */
function makeElementReadOnly(elemName){
    var elem = document.getElementById(elemName)
    if(elem != null){
        elem.setAttribute("readonly","true")
    }
}

/*
 * Unsets the read only attribute of an HTML element with the provided name
 */
function makeElementEditable(elemName){
    var elem = document.getElementById(elemName)
    if(elem != null){
        elem.removeAttribute("readonly")
    }
}

/*
 * Hides an HTML element with the given name 
 */
function hideElement(elemName){
    var elem = document.getElementById(elemName)
    if(elem != null){
        elem.style.display='none'
    }
}

/*
 * Shows an HTML element with the given name
 */
function showElement(elemName){
    var elem = document.getElementById(elemName)
    if(elem != null){
        elem.style.display='inline'
    }
}

/*
 * Disables an HTML element with the given name
 */
function disableElement(elemName){
    var elem = document.getElementById(elemName)
    if(elem != null){
        elem.disabled=true
    }
}

/*
 * Enables an HTML element with the given name
 */
function enableElement(elemName){
    var elem = document.getElementById(elemName)
    if(elem != null){
        elem.disabled=false
    }
}

/*
 * Splits a comma delimited string into tokens
 */
function splitArray(data){
    if (data == null){
        return new Array(); 
    }else{
        var tokens = String(data).split(",")
        return tokens
    }
}

/*
 * Adds an entry into the select box with the provided name 
 */
function addOptionToList(selectBoxName,text,value){ 
    var selectBox = document.getElementById(selectBoxName)
    var optn = document.createElement("OPTION");
    optn.text = text;
    optn.value = value;
    selectBox.options.add(optn);
}

/*
 * Removes an entry from the select box with the provided name 
 */
function removeOptionFromList(selectBoxName,value){
    var selectBox = document.getElementById(selectBoxName) 
    for(var i = 0; i < selectBox.options.length;i++){ 
        if(selectBox.options[i].value == value){
            selectBox.options.remove(selectBox.options[i])
            break;
        }
    }
}

/*
 * Sets the currently selected index of a select box element with
 * the given name
 */
function setSelectedComboIndex(selectBoxName, index){
    document.getElementById(selectBoxName).selectedIndex=index;
}

/*
 * Gets the selected index the currently selected item of the select box
 * with the given name
 */ 
function getSelectedComboIndex(selectBoxName){
    document.getElementById(selectBoxName).selectedIndex
}

/*
 * Clears the select box with the given name
 */
function clearComboBox(selectBoxName){
    var myList = document.getElementById(selectBoxName);
    if(myList != null){
        for(var count = myList.options.length - 1; count >= 0; count--)
        {
            myList.options[count] = null;
        }
    }
}

/*
 * Gets the value of the currently selected index of the select box of the provided
 * name
 */
function getSelectedComboValue(selectBoxName){
    var combo = document.getElementById(selectBoxName);
    return combo.options[combo.selectedIndex].value;
}

/*
 * Makes all the input fields read only
 */
function disableInput(){
    var elements = document.getElementsByTagName("*");
    var elementId = null;
    for(var i = 0; i < elements.length; i++){ 
        elementId = elements[i].id;
        if(elementId.endsWith("Text") || elementId.endsWith("Select")){
            makeElementReadOnly(elementId);
        }
    }
}

/*
 * Makes all the input fields read/write
 */
function enableInput(){

    var elements = document.getElementsByTagName("*");
    var elementId = null;
    for(var i = 0; i < elements.length; i++){ 
        elementId = elements[i].id;
        if(elementId.endsWith("Text") || elementId.endsWith("Select")){
            makeElementEditable(elementId);
        }
    }
}

/*
 * Function for testing if a string starts with a prefix 
 */
String.prototype.startsWith = function(prefix) {
    return this.indexOf(prefix) === 0;
}

/*
 * Function for testing if a string ends with a suffix
 */
String.prototype.endsWith = function(suffix) {
    return this.match(suffix+"$") == suffix;
};

/*
 * Function for testing if a string is blank 
 */
String.prototype.isBlank = function() {
    return (!this || /^\s*$/.test(this));
};

/*
 * Function for testing if an array contains an item
 */
Array.prototype.contains = function(item){
    return this.indexOf(item)!=-1
}

    