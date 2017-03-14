CREATE FUNCTION taxonomyelementtype_classificationnode_update() RETURNS void AS $$
    DECLARE
        t bool;
    BEGIN
        SELECT EXISTS(
    SELECT * FROM information_schema.tables 
    WHERE 
      table_schema = 'ebxml' AND 
      table_name = 'taxonomyelementtype_classificationnode'
) into t;
    IF 
	t ='t'
    THEN 
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SPARQL';
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SQL-92';
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:XQuery';
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:QueryLanguage:EJBQL';
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:query:ExportObject';
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:query:FindAllMyObjects';
	delete from ebxml.taxonomyelementtype_classificationnode where classificationnode_id='urn:oasis:names:tc:ebxml-regrep:query:ExtrinsicObjectQuery';
	INSERT INTO ebxml.taxonomyelementtype_classificationnode(taxonomyelementtype_id,classificationnode_id) 
	VALUES('urn:oasis:names:tc:ebxml-regrep:classificationScheme:QueryLanguage','urn:oasis:names:tc:ebxml-regrep:QueryLanguage:HQL');
	RAISE NOTICE 'updated ebxml.taxonomyelementtype_classificationnode table, success!';
    ELSE
	RAISE NOTICE 'Table ebxml.taxonomyelementtype_classificationnode does not exist, skipping!';
    END IF;
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION classificationnode_update() RETURNS void AS $$
    DECLARE
        t bool;
    BEGIN
        SELECT EXISTS(
    SELECT * FROM information_schema.tables 
    WHERE 
      table_schema = 'ebxml' AND 
      table_name = 'classificationnode'
) into t;
    IF 
	t ='t'
    THEN 
	delete from  where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SPARQL';
	delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:SQL-92';
	delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:XQuery';
	delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:EJBQL';
	delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:query:ExportObject';
	delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:query:FindAllMyObjects';
	delete from ebxml.classificationnode where id= 'urn:oasis:names:tc:ebxml-regrep:query:ExtrinsicObjectQuery';
	INSERT INTO ebxml.classificationnode (id,lid,objecttype,owner,versionname,code,parent,path) 
	VALUES ('urn:oasis:names:tc:ebxml-regrep:QueryLanguage:HQL','urn:oasis:names:tc:ebxml-regrep:QueryLanguage:HQL',
	'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:ClassificationNode','NCF','1','HQL',
	'urn:oasis:names:tc:ebxml-regrep:classificationScheme:QueryLanguage','/urn:oasis:names:tc:ebxml-regrep:classificationScheme:QueryLanguage/HQL');
	RAISE NOTICE 'updated ebxml.classificationnode table, success!';
    ELSE
	RAISE NOTICE 'Table ebxml.classificationnode does not exist, skipping!';
    END IF;
    END;
$$ LANGUAGE plpgsql;

select taxonomyelementtype_classificationnode_update();
select classificationnode_update();

DROP FUNCTION taxonomyelementtype_classificationnode_update();
DROP FUNCTION classificationnode_update();

