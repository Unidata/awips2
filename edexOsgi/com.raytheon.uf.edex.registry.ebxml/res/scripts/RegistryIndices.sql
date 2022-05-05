CREATE INDEX event_id_idx
  ON ebxml.auditableevent_action
  USING btree
  (auditableevent_id);
  
CREATE INDEX notification_id_idx
  ON ebxml.notification_auditableevent
  USING btree
  (notification_id);
  
CREATE INDEX objectreflist_id_idx
  ON ebxml.objectreflist_objectref
  USING btree
  (objectreflist_id);
  
CREATE INDEX organization_id_email_idx
  ON ebxml.organization_emailaddress
  USING btree
  (organization_id);
  
CREATE INDEX organization_id_org_idx
  ON ebxml.organization_organization
  USING btree
  (org_id);
  
CREATE INDEX organization_id_address_idx
  ON ebxml.organization_postaladdress
  USING btree
  (organization_id);
  
CREATE INDEX organization_id_telephone_idx
  ON ebxml.organization_telephonenumber
  USING btree
  (organization_id);
  
CREATE INDEX person_id_email_idx
  ON ebxml.person_emailaddress
  USING btree
  (person_id);
  
CREATE INDEX person_id_address_idx
  ON ebxml.person_postaladdress
  USING btree
  (person_id);
  
CREATE INDEX person_id_telephone_idx
  ON ebxml.person_telephonenumber
  USING btree
  (person_id);
  
CREATE INDEX querydefinition_id_idx
  ON ebxml.querydefinition_parameter
  USING btree
  (querydefinition_id);
  
CREATE INDEX registryobject_id_classification_idx
  ON ebxml.registryobject_classification
  USING btree
  (registryobject_id);
  
CREATE INDEX registryobject_id_externalid_idx
  ON ebxml.registryobject_externalidentifier
  USING btree
  (registryobject_id);
  
CREATE INDEX registryobject_id_externallink_idx
  ON ebxml.registryobject_externallink
  USING btree
  (registryobject_id);
  
CREATE INDEX registryobjectlist_id_idx
  ON ebxml.registryobjectlist_registryobject
  USING btree
  (registryobjectlist_id);
  
CREATE INDEX service_id_idx
  ON ebxml.service_serviceendpoint
  USING btree
  (service_id);
  
CREATE INDEX subscription_id_idx
  ON ebxml.subscription_deliveryinfo
  USING btree
  (subscription_id);
  
CREATE INDEX taxonomyelement_id_idx
  ON ebxml.taxonomyelementtype_classificationnode
  USING btree
  (taxonomyelementtype_id);
  
CREATE INDEX collectionvalue_id_idx
  ON ebxml.value_value
  USING btree
  (collectionvalue_id);

CREATE INDEX objectref_id_idx
  ON ebxml.objectref
  USING btree
  (id COLLATE pg_catalog."default");

CREATE INDEX objectref_key_idx
  ON ebxml.objectref
  USING btree
  (key COLLATE pg_catalog."default");

CREATE INDEX objectreflist_key_idx
  ON ebxml.objectreflist_objectref
  USING btree
  (objectref_key COLLATE pg_catalog."default");
  
CREATE INDEX slot_parent_id_idx
  ON ebxml.slot
  USING btree
  (parent_id COLLATE pg_catalog."default");
  
CREATE INDEX action_eventtype_idx
  ON ebxml.action
  USING btree
  (eventtype COLLATE pg_catalog."default");
  
CREATE INDEX auditableevent_timestamp_idx
  ON ebxml.auditableevent
  USING btree
  ("timestamp");
  
CREATE INDEX value_id_idx
  ON ebxml.value_value
  USING btree
  (value_id);
  
CREATE INDEX floatvalue_index 
   ON ebxml.value USING btree (floatvalue) 
   WHERE floatvalue IS NOT NULL;
  
CREATE INDEX integervalue_index 
   ON ebxml.value USING btree (integervalue) 
   WHERE integervalue IS NOT NULL;
  
CREATE INDEX durationvalue_index
   ON ebxml.value USING btree (durationvalue) 
   WHERE durationvalue IS NOT NULL;
  
CREATE INDEX datetimevalue_index 
   ON ebxml.value USING btree (datetimevalue) 
   WHERE datetimevalue IS NOT NULL;
  
CREATE INDEX anyvalue_index 
   ON ebxml.value USING btree (anyvalue COLLATE pg_catalog."default") 
   WHERE anyvalue IS NOT NULL;
  
CREATE INDEX booleanvalue_index 
   ON ebxml.value USING btree (booleanvalue) 
   WHERE booleanvalue IS NOT NULL;

  
