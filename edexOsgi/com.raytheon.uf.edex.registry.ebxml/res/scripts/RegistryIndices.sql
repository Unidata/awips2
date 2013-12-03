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
  
CREATE INDEX value_id_idx
  ON ebxml.value_value
  USING btree
  (value_id);