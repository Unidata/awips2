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
 *  Redirects browser to the user details page for the given user ID
 */
function getUserDetails(userId){
	window.location.href="RegistryUserInterface.html?id="+userId+"&mode=view"
}

/*
 * Redirects the browser to the organization details page for the given organization ID
 */
function getOrgDetails(userId){
	window.location.href="RegistryOrganizationInterface.html?id="+userId+"&mode=view"
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
 * Populates the country select combo box
 */
function populateCountries(selectBoxName) {
	addOptionToList(selectBoxName,"","");
	addOptionToList(selectBoxName,"USA","USA");
	addOptionToList(selectBoxName,"Afghanistan","Afghanistan");
	addOptionToList(selectBoxName,"Albania","Albania");
	addOptionToList(selectBoxName,"Algeria","Algeria");
	addOptionToList(selectBoxName,"American Samoa","American Samoa");
	addOptionToList(selectBoxName,"Andorra","Andorra");
	addOptionToList(selectBoxName,"Angola","Angola");
	addOptionToList(selectBoxName,"Anguilla","Anguilla");
	addOptionToList(selectBoxName,"Antarctica","Antarctica");
	addOptionToList(selectBoxName,"Antigua and Barbuda","Antigua and Barbuda");
	addOptionToList(selectBoxName,"Arctic Ocean","Arctic Ocean");
	addOptionToList(selectBoxName,"Argentina","Argentina");
	addOptionToList(selectBoxName,"Armenia","Armenia");
	addOptionToList(selectBoxName,"Aruba","Aruba");
	addOptionToList(selectBoxName,"Ashmore and Cartier Islands","Ashmore and Cartier Islands");
	addOptionToList(selectBoxName,"Atlantic Ocean","Atlantic Ocean");
	addOptionToList(selectBoxName,"Australia","Australia");
	addOptionToList(selectBoxName,"Austria","Austria");
	addOptionToList(selectBoxName,"Azerbaijan","Azerbaijan");
	addOptionToList(selectBoxName,"Bahamas","Bahamas");
	addOptionToList(selectBoxName,"Bahrain","Bahrain");
	addOptionToList(selectBoxName,"Baltic Sea","Baltic Sea");
	addOptionToList(selectBoxName,"Baker Island","Baker Island");
	addOptionToList(selectBoxName,"Bangladesh","Bangladesh");
	addOptionToList(selectBoxName,"Barbados","Barbados");
	addOptionToList(selectBoxName,"Bassas da India","Bassas da India");
	addOptionToList(selectBoxName,"Belarus","Belarus");
	addOptionToList(selectBoxName,"Belgium","Belgium");
	addOptionToList(selectBoxName,"Belize","Belize");
	addOptionToList(selectBoxName,"Benin","Benin");
	addOptionToList(selectBoxName,"Bermuda","Bermuda");
	addOptionToList(selectBoxName,"Bhutan","Bhutan");
	addOptionToList(selectBoxName,"Bolivia","Bolivia");
	addOptionToList(selectBoxName,"Borneo","Borneo");
	addOptionToList(selectBoxName,"Bosnia and Herzegovina","Bosnia and Herzegovina");
	addOptionToList(selectBoxName,"Botswana","Botswana");
	addOptionToList(selectBoxName,"Bouvet Island","Bouvet Island");
	addOptionToList(selectBoxName,"Brazil","Brazil");
	addOptionToList(selectBoxName,"British Virgin Islands","British Virgin Islands");
	addOptionToList(selectBoxName,"Brunei","Brunei");
	addOptionToList(selectBoxName,"Bulgaria","Bulgaria");
	addOptionToList(selectBoxName,"Burkina Faso","Burkina Faso");
	addOptionToList(selectBoxName,"Burundi","Burundi");
	addOptionToList(selectBoxName,"Cambodia","Cambodia");
	addOptionToList(selectBoxName,"Cameroon","Cameroon");
	addOptionToList(selectBoxName,"Canada","Canada");
	addOptionToList(selectBoxName,"Cape Verde","Cape Verde");
	addOptionToList(selectBoxName,"Cayman Islands","Cayman Islands");
	addOptionToList(selectBoxName,"Central African Republic","Central African Republic");
	addOptionToList(selectBoxName,"Chad","Chad");
	addOptionToList(selectBoxName,"Chile","Chile");
	addOptionToList(selectBoxName,"China","China");
	addOptionToList(selectBoxName,"Christmas Island","Christmas Island");
	addOptionToList(selectBoxName,"Clipperton Island","Clipperton Island");
	addOptionToList(selectBoxName,"Cocos Islands","Cocos Islands");
	addOptionToList(selectBoxName,"Colombia","Colombia");
	addOptionToList(selectBoxName,"Comoros","Comoros");
	addOptionToList(selectBoxName,"Cook Islands","Cook Islands");
	addOptionToList(selectBoxName,"Coral Sea Islands","Coral Sea Islands");
	addOptionToList(selectBoxName,"Costa Rica","Costa Rica");
	addOptionToList(selectBoxName,"Cote d'Ivoire","Cote d'Ivoire");
	addOptionToList(selectBoxName,"Croatia","Croatia");
	addOptionToList(selectBoxName,"Cuba","Cuba");
	addOptionToList(selectBoxName,"Curacao","Curacao");
	addOptionToList(selectBoxName,"Cyprus","Cyprus");
	addOptionToList(selectBoxName,"Czech Republic","Czech Republic");
	addOptionToList(selectBoxName,"Democratic Republic of the Congo","Democratic Republic of the Congo");
	addOptionToList(selectBoxName,"Denmark","Denmark");
	addOptionToList(selectBoxName,"Djibouti","Djibouti");
	addOptionToList(selectBoxName,"Dominica","Dominica");
	addOptionToList(selectBoxName,"Dominican Republic","Dominican Republic");
	addOptionToList(selectBoxName,"East Timor","East Timor");
	addOptionToList(selectBoxName,"Ecuador","Ecuador");
	addOptionToList(selectBoxName,"Egypt","Egypt");
	addOptionToList(selectBoxName,"El Salvador","El Salvador");
	addOptionToList(selectBoxName,"Equatorial Guinea","Equatorial Guinea");
	addOptionToList(selectBoxName,"Eritrea","Eritrea");
	addOptionToList(selectBoxName,"Estonia","Estonia");
	addOptionToList(selectBoxName,"Ethiopia","Ethiopia");
	addOptionToList(selectBoxName,"Europa Island","Europa Island");
	addOptionToList(selectBoxName,"Falkland Islands (Islas Malvinas)","Falkland Islands (Islas Malvinas)");
	addOptionToList(selectBoxName,"Faroe Islands","Faroe Islands");
	addOptionToList(selectBoxName,"Fiji","Fiji");
	addOptionToList(selectBoxName,"Finland","Finland");
	addOptionToList(selectBoxName,"France","France");
	addOptionToList(selectBoxName,"French Guiana","French Guiana");
	addOptionToList(selectBoxName,"French Polynesia","French Polynesia");
	addOptionToList(selectBoxName,"French Southern and Antarctic Lands","French Southern and Antarctic Lands");
	addOptionToList(selectBoxName,"Gabon","Gabon");
	addOptionToList(selectBoxName,"Gambia","Gambia");
	addOptionToList(selectBoxName,"Gaza Strip","Gaza Strip");
	addOptionToList(selectBoxName,"Georgia","Georgia");
	addOptionToList(selectBoxName,"Germany","Germany");
	addOptionToList(selectBoxName,"Ghana","Ghana");
	addOptionToList(selectBoxName,"Gibraltar","Gibraltar");
	addOptionToList(selectBoxName,"Glorioso Islands","Glorioso Islands");
	addOptionToList(selectBoxName,"Greece","Greece");
	addOptionToList(selectBoxName,"Greenland","Greenland");
	addOptionToList(selectBoxName,"Grenada","Grenada");
	addOptionToList(selectBoxName,"Guadeloupe","Guadeloupe");
	addOptionToList(selectBoxName,"Guam","Guam");
	addOptionToList(selectBoxName,"Guatemala","Guatemala");
	addOptionToList(selectBoxName,"Guernsey","Guernsey");
	addOptionToList(selectBoxName,"Guinea","Guinea");
	addOptionToList(selectBoxName,"Guinea-Bissau","Guinea-Bissau");
	addOptionToList(selectBoxName,"Guyana","Guyana");
	addOptionToList(selectBoxName,"Haiti","Haiti");
	addOptionToList(selectBoxName,"Heard Island and McDonald Islands","Heard Island and McDonald Islands");
	addOptionToList(selectBoxName,"Honduras","Honduras");
	addOptionToList(selectBoxName,"Hong Kong","Hong Kong");
	addOptionToList(selectBoxName,"Howland Island","Howland Island");
	addOptionToList(selectBoxName,"Hungary","Hungary");
	addOptionToList(selectBoxName,"Iceland","Iceland");
	addOptionToList(selectBoxName,"India","India");
	addOptionToList(selectBoxName,"Indian Ocean","Indian Ocean");
	addOptionToList(selectBoxName,"Indonesia","Indonesia");
	addOptionToList(selectBoxName,"Iran","Iran");
	addOptionToList(selectBoxName,"Iraq","Iraq");
	addOptionToList(selectBoxName,"Ireland","Ireland");
	addOptionToList(selectBoxName,"Isle of Man","Isle of Man");
	addOptionToList(selectBoxName,"Israel","Israel");
	addOptionToList(selectBoxName,"Italy","Italy");
	addOptionToList(selectBoxName,"Jamaica","Jamaica");
	addOptionToList(selectBoxName,"Jan Mayen","Jan Mayen");
	addOptionToList(selectBoxName,"Japan","Japan");
	addOptionToList(selectBoxName,"Jarvis Island","Jarvis Island");
	addOptionToList(selectBoxName,"Jersey","Jersey");
	addOptionToList(selectBoxName,"Johnston Atoll","Johnston Atoll");
	addOptionToList(selectBoxName,"Jordan","Jordan");
	addOptionToList(selectBoxName,"Juan de Nova Island","Juan de Nova Island");
	addOptionToList(selectBoxName,"Kazakhstan","Kazakhstan");
	addOptionToList(selectBoxName,"Kenya","Kenya");
	addOptionToList(selectBoxName,"Kerguelen Archipelago","Kerguelen Archipelago");
	addOptionToList(selectBoxName,"Kingman Reef","Kingman Reef");
	addOptionToList(selectBoxName,"Kiribati","Kiribati");
	addOptionToList(selectBoxName,"Kosovo","Kosovo");
	addOptionToList(selectBoxName,"Kuwait","Kuwait");
	addOptionToList(selectBoxName,"Kyrgyzstan","Kyrgyzstan");
	addOptionToList(selectBoxName,"Laos","Laos");
	addOptionToList(selectBoxName,"Latvia","Latvia");
	addOptionToList(selectBoxName,"Lebanon","Lebanon");
	addOptionToList(selectBoxName,"Lesotho","Lesotho");
	addOptionToList(selectBoxName,"Liberia","Liberia");
	addOptionToList(selectBoxName,"Libya","Libya");
	addOptionToList(selectBoxName,"Liechtenstein","Liechtenstein");
	addOptionToList(selectBoxName,"Lithuania","Lithuania");
	addOptionToList(selectBoxName,"Luxembourg","Luxembourg");
	addOptionToList(selectBoxName,"Macau","Macau");
	addOptionToList(selectBoxName,"Macedonia","Macedonia");
	addOptionToList(selectBoxName,"Madagascar","Madagascar");
	addOptionToList(selectBoxName,"Malawi","Malawi");
	addOptionToList(selectBoxName,"Malaysia","Malaysia");
	addOptionToList(selectBoxName,"Maldives","Maldives");
	addOptionToList(selectBoxName,"Mali","Mali");
	addOptionToList(selectBoxName,"Malta","Malta");
	addOptionToList(selectBoxName,"Marshall Islands","Marshall Islands");
	addOptionToList(selectBoxName,"Martinique","Martinique");
	addOptionToList(selectBoxName,"Mauritania","Mauritania");
	addOptionToList(selectBoxName,"Mauritius","Mauritius");
	addOptionToList(selectBoxName,"Mayotte","Mayotte");
	addOptionToList(selectBoxName,"Mediterranean Sea","Mediterranean Sea");
	addOptionToList(selectBoxName,"Mexico","Mexico");
	addOptionToList(selectBoxName,"Micronesia","Micronesia");
	addOptionToList(selectBoxName,"Midway Islands","Midway Islands");
	addOptionToList(selectBoxName,"Moldova","Moldova");
	addOptionToList(selectBoxName,"Monaco","Monaco");
	addOptionToList(selectBoxName,"Mongolia","Mongolia");
	addOptionToList(selectBoxName,"Montenegro","Montenegro");
	addOptionToList(selectBoxName,"Montserrat","Montserrat");
	addOptionToList(selectBoxName,"Morocco","Morocco");
	addOptionToList(selectBoxName,"Mozambique","Mozambique");
	addOptionToList(selectBoxName,"Myanmar","Myanmar");
	addOptionToList(selectBoxName,"Namibia","Namibia");
	addOptionToList(selectBoxName,"Nauru","Nauru");
	addOptionToList(selectBoxName,"Navassa Island","Navassa Island");
	addOptionToList(selectBoxName,"Nepal","Nepal");
	addOptionToList(selectBoxName,"Netherlands","Netherlands");
	addOptionToList(selectBoxName,"New Caledonia","New Caledonia");
	addOptionToList(selectBoxName,"New Zealand","New Zealand");
	addOptionToList(selectBoxName,"Nicaragua","Nicaragua");
	addOptionToList(selectBoxName,"Niger","Niger");
	addOptionToList(selectBoxName,"Nigeria","Nigeria");
	addOptionToList(selectBoxName,"Niue","Niue");
	addOptionToList(selectBoxName,"Norfolk Island","Norfolk Island");
	addOptionToList(selectBoxName,"North Korea","North Korea");
	addOptionToList(selectBoxName,"North Sea","North Sea");
	addOptionToList(selectBoxName,"Northern Mariana Islands","Northern Mariana Islands");
	addOptionToList(selectBoxName,"Norway","Norway");
	addOptionToList(selectBoxName,"Oman","Oman");
	addOptionToList(selectBoxName,"Pacific Ocean","Pacific Ocean");
	addOptionToList(selectBoxName,"Pakistan","Pakistan");
	addOptionToList(selectBoxName,"Palau","Palau");
	addOptionToList(selectBoxName,"Palmyra Atoll","Palmyra Atoll");
	addOptionToList(selectBoxName,"Panama","Panama");
	addOptionToList(selectBoxName,"Papua New Guinea","Papua New Guinea");
	addOptionToList(selectBoxName,"Paracel Islands","Paracel Islands");
	addOptionToList(selectBoxName,"Paraguay","Paraguay");
	addOptionToList(selectBoxName,"Peru","Peru");
	addOptionToList(selectBoxName,"Philippines","Philippines");
	addOptionToList(selectBoxName,"Pitcairn Islands","Pitcairn Islands");
	addOptionToList(selectBoxName,"Poland","Poland");
	addOptionToList(selectBoxName,"Portugal","Portugal");
	addOptionToList(selectBoxName,"Puerto Rico","Puerto Rico");
	addOptionToList(selectBoxName,"Qatar","Qatar");
	addOptionToList(selectBoxName,"Republic of the Congo","Republic of the Congo");
	addOptionToList(selectBoxName,"Reunion","Reunion");
	addOptionToList(selectBoxName,"Romania","Romania");
	addOptionToList(selectBoxName,"Ross Sea","Ross Sea");
	addOptionToList(selectBoxName,"Russia","Russia");
	addOptionToList(selectBoxName,"Rwanda","Rwanda");
	addOptionToList(selectBoxName,"Saint Helena","Saint Helena");
	addOptionToList(selectBoxName,"Saint Kitts and Nevis","Saint Kitts and Nevis");
	addOptionToList(selectBoxName,"Saint Lucia","Saint Lucia");
	addOptionToList(selectBoxName,"Saint Pierre and Miquelon","Saint Pierre and Miquelon");
	addOptionToList(selectBoxName,"Saint Vincent and the Grenadines","Saint Vincent and the Grenadines");
	addOptionToList(selectBoxName,"Samoa","Samoa");
	addOptionToList(selectBoxName,"San Marino","San Marino");
	addOptionToList(selectBoxName,"Sao Tome and Principe","Sao Tome and Principe");
	addOptionToList(selectBoxName,"Saudi Arabia","Saudi Arabia");
	addOptionToList(selectBoxName,"Senegal","Senegal");
	addOptionToList(selectBoxName,"Serbia","Serbia");
	addOptionToList(selectBoxName,"Seychelles","Seychelles");
	addOptionToList(selectBoxName,"Sierra Leone","Sierra Leone");
	addOptionToList(selectBoxName,"Singapore","Singapore");
	addOptionToList(selectBoxName,"Sint Maarten","Sint Maarten");
	addOptionToList(selectBoxName,"Slovakia","Slovakia");
	addOptionToList(selectBoxName,"Slovenia","Slovenia");
	addOptionToList(selectBoxName,"Solomon Islands","Solomon Islands");
	addOptionToList(selectBoxName,"Somalia","Somalia");
	addOptionToList(selectBoxName,"South Africa","South Africa");
	addOptionToList(selectBoxName,"South Georgia and the South Sandwich Islands","South Georgia and the South Sandwich Islands");
	addOptionToList(selectBoxName,"South Korea","South Korea");
	addOptionToList(selectBoxName,"Southern Ocean","Southern Ocean");
	addOptionToList(selectBoxName,"Spain","Spain");
	addOptionToList(selectBoxName,"Spratly Islands","Spratly Islands");
	addOptionToList(selectBoxName,"Sri Lanka","Sri Lanka");
	addOptionToList(selectBoxName,"Sudan","Sudan");
	addOptionToList(selectBoxName,"Suriname","Suriname");
	addOptionToList(selectBoxName,"Svalbard","Svalbard");
	addOptionToList(selectBoxName,"Swaziland","Swaziland");
	addOptionToList(selectBoxName,"Sweden","Sweden");
	addOptionToList(selectBoxName,"Switzerland","Switzerland");
	addOptionToList(selectBoxName,"Syria","Syria");
	addOptionToList(selectBoxName,"Taiwan","Taiwan");
	addOptionToList(selectBoxName,"Tajikistan","Tajikistan");
	addOptionToList(selectBoxName,"Tanzania","Tanzania");
	addOptionToList(selectBoxName,"Tasman Sea","Tasman Sea");
	addOptionToList(selectBoxName,"Thailand","Thailand");
	addOptionToList(selectBoxName,"Togo","Togo");
	addOptionToList(selectBoxName,"Tokelau","Tokelau");
	addOptionToList(selectBoxName,"Tonga","Tonga");
	addOptionToList(selectBoxName,"Trinidad and Tobago","Trinidad and Tobago");
	addOptionToList(selectBoxName,"Tromelin Island","Tromelin Island");
	addOptionToList(selectBoxName,"Tunisia","Tunisia");
	addOptionToList(selectBoxName,"Turkey","Turkey");
	addOptionToList(selectBoxName,"Turkmenistan","Turkmenistan");
	addOptionToList(selectBoxName,"Turks and Caicos Islands","Turks and Caicos Islands");
	addOptionToList(selectBoxName,"Tuvalu","Tuvalu");
	addOptionToList(selectBoxName,"Uganda","Uganda");
	addOptionToList(selectBoxName,"Ukraine","Ukraine");
	addOptionToList(selectBoxName,"United Arab Emirates","United Arab Emirates");
	addOptionToList(selectBoxName,"United Kingdom","United Kingdom");
	addOptionToList(selectBoxName,"Uruguay","Uruguay");
	addOptionToList(selectBoxName,"Uzbekistan","Uzbekistan");
	addOptionToList(selectBoxName,"Vanuatu","Vanuatu");
	addOptionToList(selectBoxName,"Venezuela","Venezuela");
	addOptionToList(selectBoxName,"Viet Nam","Viet Nam");
	addOptionToList(selectBoxName,"Virgin Islands","Virgin Islands");
	addOptionToList(selectBoxName,"Wake Island","Wake Island");
	addOptionToList(selectBoxName,"Wallis and Futuna","Wallis and Futuna");
	addOptionToList(selectBoxName,"West Bank","West Bank");
	addOptionToList(selectBoxName,"Western Sahara","Western Sahara");
	addOptionToList(selectBoxName,"Yemen","Yemen");
	addOptionToList(selectBoxName,"Zambia","Zambia");
	addOptionToList(selectBoxName,"Zimbabwe","Zimbabwe");
	setSelectedComboIndex(selectBoxName,1);
} 

/*
 * Populates the states for the provided country
 */
function populateStates(stateSelectBox, countrySelectBox){
	getSelectedComboValue("countrySelect")
	if(getSelectedComboValue("countrySelect")!="USA"){
		clearComboBox("stateSelect");
	}else{
		addOptionToList(stateSelectBox,"","");
		addOptionToList(stateSelectBox,"AL - Alabama","AL");  
		addOptionToList(stateSelectBox,"AK - Alaska","AK");
		addOptionToList(stateSelectBox,"AZ - Arizona","AZ");
		addOptionToList(stateSelectBox,"AR - Arkansas","AR");
		addOptionToList(stateSelectBox,"CA - California","CA");
		addOptionToList(stateSelectBox,"CO - Colorado","CO");
		addOptionToList(stateSelectBox,"CT - Connecticut","CT");
		addOptionToList(stateSelectBox,"DC - District of Columbia","DC");
		addOptionToList(stateSelectBox,"DE - Delaware","DE");
		addOptionToList(stateSelectBox,"FL - Florida","FL");
		addOptionToList(stateSelectBox,"GA - Georgia","GA");
		addOptionToList(stateSelectBox,"HI - Hawaii","HI");
		addOptionToList(stateSelectBox,"ID - Idaho","ID");
		addOptionToList(stateSelectBox,"IL - Illinois","IL");
		addOptionToList(stateSelectBox,"IN - Indiana","IN");
		addOptionToList(stateSelectBox,"IA - Iowa","IA");
		addOptionToList(stateSelectBox,"KS - Kansas","KS");
		addOptionToList(stateSelectBox,"KY - Kentucky","KY");
		addOptionToList(stateSelectBox,"LA - Louisiana","LA");
		addOptionToList(stateSelectBox,"ME - Maine","ME");
		addOptionToList(stateSelectBox,"MD - Maryland","MD");
		addOptionToList(stateSelectBox,"MA - Massachusetts","MA");
		addOptionToList(stateSelectBox,"MI - Michigan","MI");
		addOptionToList(stateSelectBox,"MN - Minnesota","MN");
		addOptionToList(stateSelectBox,"MS - Mississippi","MS");
		addOptionToList(stateSelectBox,"MO - Missouri","MO");
		addOptionToList(stateSelectBox,"MT - Montana","MT");
		addOptionToList(stateSelectBox,"NE - Nebraska","NE");
		addOptionToList(stateSelectBox,"NV - Nevada","NV");
		addOptionToList(stateSelectBox,"NH - New Hampshire","NH");
		addOptionToList(stateSelectBox,"NJ - New Jersey","NJ");
		addOptionToList(stateSelectBox,"NM - New Mexico","NM");
		addOptionToList(stateSelectBox,"NY - New York","NY");
		addOptionToList(stateSelectBox,"NC - North Carolina","NC");
		addOptionToList(stateSelectBox,"ND - North Dakota","ND");
		addOptionToList(stateSelectBox,"OH - Ohio","OH");
		addOptionToList(stateSelectBox,"OK - Oklahoma","OK");
		addOptionToList(stateSelectBox,"OR - Oregon","OR");
		addOptionToList(stateSelectBox,"PA - Pennsylvania","PA");
		addOptionToList(stateSelectBox,"RI - Rhode Island","RI");
		addOptionToList(stateSelectBox,"SC - South Carolina","SC");
		addOptionToList(stateSelectBox,"SD - South Dakota","SD");
		addOptionToList(stateSelectBox,"TN - Tennessee","TN");
		addOptionToList(stateSelectBox,"TX - Texas","TX");
		addOptionToList(stateSelectBox,"UT - Utah","UT");
		addOptionToList(stateSelectBox,"VT - Vermont","VT");
		addOptionToList(stateSelectBox,"VA - Virginia","VA");
		addOptionToList(stateSelectBox,"WA - Washington","WA");
		addOptionToList(stateSelectBox,"WV - West Virginia","WV");
		addOptionToList(stateSelectBox,"WI - Wisconsin","WI");
		addOptionToList(stateSelectBox,"WY - Wyoming","WY"); 
	}
}

/*
 * This function examines the mode and executes the appropriate action
 */
function partyAction(){ 
	if(self.mode=="add"){
		addParty()
	} else if (self.mode=="modify"){
		modifyParty()
	}
}

/*
 * Posts a message to the server to add a user to the registry
 */
function addParty(){
	var formValues = getFormValues();
	if(validateFormValues(formValues)){
		submitPostRequest("AddRegistryParty",formValues)
	}
}

/*
 * Posts a message to the server to modify the current user based
 * on the values in the form
 */
function modifyParty(){
	var formValues = getFormValues();
	if(validateFormValues(formValues)){
		submitPostRequest("ModifyRegistryParty",formValues)
	}
}

/*
 * Posts a message to the server to delete the current user
 */
function deleteParty(){
	submitPostRequest("DeleteRegistryParty",getFormValues())
}
	
	
/*
 * Sends requests to the server to populate the dynamic combo boxes as well as populating
 * predefined data such as countries and states
 */
function populateFormData(type){
	self.objType=type
	populateCountries("countrySelect");
	populateStates("stateSelect", "countrySelect");
	RegistryWeb.getAddressTypes(function(addrs){
		var addressTypes = splitArray(addrs)
		addOptionToList("addressTypeSelect","","");
		for(var i=0;i<addressTypes.length;i++){
			addOptionToList("addressTypeSelect",addressTypes[i],addressTypes[i]);
		}
		// Next, we get the telephone number types
		RegistryWeb.getTelephoneTypes(function(tellyTypes){
			var phoneTypes = splitArray(tellyTypes)
			addOptionToList("telephoneTypeSelect","","");
			for(var i=0;i<phoneTypes.length;i++){
				addOptionToList("telephoneTypeSelect",phoneTypes[i],phoneTypes[i]);
			}
			RegistryWeb.getEmailTypes(function(emails){
				var emailTypes = splitArray(emails)
				addOptionToList("emailTypeSelect","","");
				for(var i=0;i<emailTypes.length;i++){
					addOptionToList("emailTypeSelect",emailTypes[i],emailTypes[i]);
				}
				populateUserData();
			});
		});
	});
	
}	
	
/*
 * Populates the form based on the current mode of the page
 */
function populateUserData(){ 

    /*
     * If we are in view mode, the information for the current user is requested via DWR.
     * The user action button is hidden and the modify and delete buttons are enabled.  The 
     * fields are set to read only since we are simply viewing the data
     */
	if(self.mode=="view"){
		document.getElementById("titleSpan").innerHTML="View "+self.objType+" Information for: "+self.userId;
		RegistryWeb.getPartyDetails(userId,function(output){self.updateFields(output);});
		disableInput()
		hideElement("userActionButton");
		hideElement("useOrgAddressCheck");
		hideElement("useOrgAddressSpan");
		hideElement("useOrgPhoneCheck");
		hideElement("useOrgPhoneSpan");
		hideElement("useOrgEmailCheck");
		hideElement("useOrgEmailSpan");
		enableElement("modifyPartyButton");
		enableElement("deletePartyButton");
	}
	/*
	 * If we are in add mode, the input fields are set to read/write, the user action button 
	 * is enabled and the modify user and delete user buttons are disabled since they do not
	 * apply to this mode.
	 */
	else if (self.mode=="add"){
		document.getElementById("titleSpan").innerHTML="Add New "+self.objType;
		enableInput()
		showElement("userActionButton")
		showElement("useOrgAddressCheck")
		showElement("useOrgAddressSpan");
		showElement("useOrgPhoneCheck");
		showElement("useOrgPhoneSpan");
		showElement("useOrgEmailCheck");
		showElement("useOrgEmailSpan");
		document.getElementById("userActionButton").innerHTML="Add "+self.objType;
		disableElement("modifyPartyButton");
		disableElement("deletePartyButton");
	}
	/*
	 * If we are in modify mode, the information for the current user is requested via DWR.
	 * The input fields are set to read/write, the user action button is enabled.
	 * The modify user button is disabled since we are already in the modify mode.  The delete
	 * user button is enabled.
	 */
	else if (self.mode=="modify"){
		document.getElementById("titleSpan").innerHTML="Modify "+self.objType+" Information for: "+self.userId;
		RegistryWeb.getPartyDetails(userId,function(output){self.updateFields(output);});
		enableInput()
		showElement("userActionButton");
		showElement("useOrgAddressCheck");
		showElement("useOrgAddressSpan");
		showElement("useOrgPhoneCheck");
		showElement("useOrgPhoneSpan");
		showElement("useOrgEmailCheck");
		showElement("useOrgEmailSpan");
		document.getElementById("userActionButton").innerHTML="Modify "+self.objType; 
		disableElement("modifyPartyButton");
		enableElement("deletePartyButton");
		makeElementReadOnly("idText");
	}
	/*
	 * If we hit this condition, a user has entered an invalid mode.  This can only be encountered if
	 * the user manually entered the url string
	 */
	else{
		alert("Invalid mode: "+self.mode)
	}  
	
}

/*
 * Takes the response from the DWR calls executed in the populateUserData() function and updates the 
 * fields appropriately
 */
function updateFields(values){
	var tokens = values.split("_____");
	for(var i = 0; i < tokens.length; i++){
		var subTokens = tokens[i].split("===");
		setElementValue(subTokens[0],subTokens[1]);
	}
}

/*
 * Extracts the values from the text fields and combo boxes and
 * places them into an object.  This object can then be used as
 * an argument for posting to the server
 */ 
function getFormValues(){ 
	formValues = new Object();
	formValues.objType=self.objType;
	formValues.action=self.mode;
	var elements = document.getElementsByTagName("*");
	var elementId = null;
	for(var i = 0; i < elements.length; i++){ 
		elementId = elements[i].id;
		if(elementId.endsWith("Text")){
			paramName = elementId.replace("Text","");
			eval("formValues."+paramName+"='"+elements[i].value+"'")
		} else if (elementId.endsWith("Select")){
			var paramValue = getComboValue(elementId);
			paramName = elementId.replace("Select","");
			eval("formValues."+paramName+"='"+paramValue+"'")
		} else if (elementId.endsWith("Span")){
			paramName = elementId.replace("Span","");
			var paramValue = getSpanValue(elementId);
			var aIdx = paramValue.indexOf("</a>");
			var startIndex = paramValue.indexOf('>');
			if(paramValue.indexOf("</a>")!=-1){
				paramValue = paramValue.substring(startIndex+1,aIdx);
			}
			
			eval("formValues."+paramName+"='"+paramValue+"'")
		}
	}
	return formValues;
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

	

