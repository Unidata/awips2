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
 * Utility methods for registry query web interface
 *
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
	function getValidQueryTypes(){  
		var tokens = callDataAccessService("getQueries").split("\n")  
		var selectBox = document.getElementById("queryTypeInput")  
		for(var i=0;i<tokens.length;i++){
			if(tokens[i].length != 0){
				var textTokens = tokens[i].split(":"); 
				var optn = document.createElement("OPTION");
				optn.text = textTokens[textTokens.length-1]; 
				optn.value = tokens[i];
				selectBox.options.add(optn); 
			}
		}
		getQueryParameters(); 
	}

	function getQueryParameters(){
		var selectBox = document.getElementById("queryTypeInput")
		var value = selectBox.options[selectBox.selectedIndex].value
		var responseTokens = callDataAccessServiceWithArg("getParametersForQuery",value).split("\n")
	
		var gen = "<table>"
		var paramType=""
		var paramName=""
		var paramDefault=""
		for(var i=0; i<responseTokens.length-1;i+=3){
			paramName = responseTokens[i]
			paramType = responseTokens[i+1]
			paramDefault = responseTokens[i+2]
			gen+="<tr><td>"+paramName+"</td><td>";
			if (paramType=="boolean"){
				if(paramDefault == "false"){
					gen+="<select name=\""+paramName+"\" id=\""+paramName+"\"><option selected value=\"false\">False</option><option value=\"true\">True</option></select>"
				}else if (paramDefault == "true"){
					gen+="<select name=\""+paramName+"\" id=\""+paramName+"\"><option value=\"false\">False</option><option selected  value=\"true\">True</option></select>"
				}else{
					gen+="<select name=\""+paramName+"\" id=\""+paramName+"\"><option value=\"false\">False</option><option value=\"true\">True</option></select>"
				}
			}else{

				if(paramDefault=="null"){
					gen+= "<input name=\""+paramName+"\" id=\""+paramName+"\"/>"
				}else{
					gen+= "<input name=\""+paramName+"\" id=\""+paramName+"\" value=\""+paramDefault+"\"/>"
				}
			}
			gen+="</td></tr>"		
		}
		gen+="</table><br><input type=\"submit\" value=\"Submit\"/>"
		document.getElementById("querySpecificSpan").innerHTML = gen
	}	