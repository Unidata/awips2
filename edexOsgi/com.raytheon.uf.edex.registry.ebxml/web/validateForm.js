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
 * Utility for validating entries on the web page before submission to the server
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
var requiredMessage ="The following fields are required:\n";

var userReqFields=['id','firstName','lastName',
                    'userOrg','userRole','addressType',
                    'streetAddress1','city','state','country',
                    'postalCode','telephoneType','areaCode','phone1','phone2',
                    'emailType','email'];
                    
var orgReqFields=['id','organizationName','addressType',
                    'streetAddress1','city','state','country',
                    'postalCode','telephoneType','areaCode','phone1','phone2',
                    'emailType','email'];

var fieldMap={};
fieldMap['id']='ID';
fieldMap['organizationName']='Organization Name';
fieldMap['firstName']='First Name';
fieldMap['middleName']='Middle Name';
fieldMap['lastName']='Last Name';
fieldMap['userOrg']='Organization';
fieldMap['userRole']='Role';
fieldMap['addressType']='Address Type';
fieldMap['streetAddress1']='Address 1';
fieldMap['streetAddress2']='Address 2';
fieldMap['city']='City';
fieldMap['state']='State/Province';
fieldMap['country']='Country';
fieldMap['postalCode']='Postal Code';
fieldMap['telephoneType']='Telephone Type';
fieldMap['areaCode']='Area Code';
fieldMap['phone1']='Telephone Prefix';
fieldMap['phone2']='Telephone Suffix';
fieldMap['extension']='Telephone Extension';
fieldMap['emailType']='Email Type';
fieldMap['email']='Email Address';

function validateFormValues(values){

	var requiredFields = null;
	if(values.objType=='User'){
		requiredFields = userReqFields;
	}else{
		requiredFields = orgReqFields;
	}

	var reqFields = "";
	for(var property in values){
		if(requiredFields.contains(property)){
			var val = eval("values."+property);
			if(val.isBlank()){
				reqFields+="\t"+fieldMap[property]+"\n";
			}
		}
	}
	if(!reqFields.isBlank()){
		alert(requiredMessage+reqFields)
		return false;
	}
	return true
}