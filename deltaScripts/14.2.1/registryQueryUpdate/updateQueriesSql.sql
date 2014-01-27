delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SPARQL';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SPARQL';

delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SQL-92';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SQL-92';

delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:XQuery';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:XQuery';

delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:EJBQL';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:EJBQL';

INSERT INTO ebxml.classificationnode (id,lid,objecttype,owner,versionname,code,parent,path) VALUES 
('urn:oasis:names:tc:ebxml-regrep:QueryLanguage:HQL','urn:oasis:names:tc:ebxml-regrep:QueryLanguage:HQL',
'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:ClassificationNode','NCF','1','HQL',
'urn:oasis:names:tc:ebxml-regrep:classificationScheme:QueryLanguage','/urn:oasis:names:tc:ebxml-regrep:classificationScheme:QueryLanguage/HQL');
INSERT INTO ebxml.taxonomyelementtype_classificationnode(taxonomyelementtype_id,classificationnode_id) VALUES('urn:oasis:names:tc:ebxml-regrep:classificationScheme:QueryLanguage','urn:oasis:names:tc:ebxml-regrep:QueryLanguage:HQL');

delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:query:ExportObject';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:query:ExportObject';

delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:query:FindAllMyObjects';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:query:FindAllMyObjects';

delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:query:ExtrinsicObjectQuery';
delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:query:ExtrinsicObjectQuery';
