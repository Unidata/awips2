#!/bin/bash

# This script should be run on the database server and drops several tables from the ebxml schema that are always
# empty and unused


echo INFO: Dropping unneccessary ebxml tables

/awips2/psql/bin/psql -U awipsadmin -d metadata -c " \

drop table if exists ebxml.workflowaction, ebxml.timeoutexceptiontype, ebxml.quotaexceededexceptiontype, ebxml.filteringexceptiontype, ebxml.catalogingexceptiontype, ebxml.authorizationexceptiontype, ebxml.authenticationexceptiontype, ebxml.unsupportedcapabilityexceptiontype, ebxml.registryrequest, ebxml.catalogobjectsrequest, ebxml.filterobjectsrequest, ebxml.queryrequest, ebxml.submitobjectsrequest, ebxml.removeobjectsrequest, ebxml.validateobjectsrequest,ebxml.updateobjectsrequest, ebxml.registryresponse, ebxml.catalogobjectsresponse, ebxml.filterobjectsresponse, ebxml.queryresponse, ebxml.validateobjectsresponse, ebxml.registryexception, ebxml.invalidrequestexceptiontype, ebxml.objectexistsexceptiontype, ebxml.objectnotfoundexceptiontype, ebxml.queryexceptiontype, ebxml.referencesexistexceptiontype, ebxml.UnresolvedReferenceExceptionType, ebxml.validationexceptiontype, ebxml.stringQueryExpression, ebxml.XMLQueryExpression, ebxml.servicebinding, ebxml.serviceinterface

"

echo INFO: Finished dropping unneccessary ebxml tables

