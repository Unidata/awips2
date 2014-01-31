CREATE INDEX event_id_idx
  ON ebxml.auditableevent_action
  USING hash
  (auditableevent_id);
  
CREATE INDEX notification_id_idx
  ON ebxml.notification_auditableevent
  USING hash
  (notification_id);
  
CREATE INDEX objectreflist_id_idx
  ON ebxml.objectreflist_objectref
  USING btree
  (objectreflist_id);
  
CREATE INDEX organization_id_email_idx
  ON ebxml.organization_emailaddress
  USING hash
  (organization_id);
  
CREATE INDEX organization_id_org_idx
  ON ebxml.organization_organization
  USING hash
  (org_id);
  
CREATE INDEX organization_id_address_idx
  ON ebxml.organization_postaladdress
  USING hash
  (organization_id);
  
CREATE INDEX organization_id_telephone_idx
  ON ebxml.organization_telephonenumber
  USING hash
  (organization_id);
  
CREATE INDEX person_id_email_idx
  ON ebxml.person_emailaddress
  USING hash
  (person_id);
  
CREATE INDEX person_id_address_idx
  ON ebxml.person_postaladdress
  USING hash
  (person_id);
  
CREATE INDEX person_id_telephone_idx
  ON ebxml.person_telephonenumber
  USING hash
  (person_id);
  
CREATE INDEX querydefinition_id_idx
  ON ebxml.querydefinition_parameter
  USING hash
  (querydefinition_id);
  
CREATE INDEX registryobject_id_classification_idx
  ON ebxml.registryobject_classification
  USING hash
  (registryobject_id);
  
CREATE INDEX registryobject_id_externalid_idx
  ON ebxml.registryobject_externalidentifier
  USING hash
  (registryobject_id);
  
CREATE INDEX registryobject_id_externallink_idx
  ON ebxml.registryobject_externallink
  USING hash
  (registryobject_id);
  
CREATE INDEX registryobjectlist_id_idx
  ON ebxml.registryobjectlist_registryobject
  USING btree
  (registryobjectlist_id);
  
CREATE INDEX service_id_idx
  ON ebxml.service_serviceendpoint
  USING hash
  (service_id);
  
CREATE INDEX subscription_id_idx
  ON ebxml.subscription_deliveryinfo
  USING hash
  (subscription_id);
  
CREATE INDEX taxonomyelement_id_idx
  ON ebxml.taxonomyelementtype_classificationnode
  USING hash
  (taxonomyelementtype_id);
  
CREATE INDEX value_id_idx
  ON ebxml.value_value
  USING btree
  (value_id);
  
CREATE INDEX collectionvalue_id_idx
  ON ebxml.value_value
  USING btree
  (collectionvalue_id);

CREATE INDEX objectref_id_idx
  ON ebxml.objectref
  USING hash
  (id COLLATE pg_catalog."default");

CREATE INDEX objectref_key_idx
  ON ebxml.objectref
  USING hash
  (key COLLATE pg_catalog."default");

CREATE INDEX objectreflist_key_idx
  ON ebxml.objectreflist_objectref
  USING hash
  (objectref_key COLLATE pg_catalog."default");
  
CREATE INDEX slot_parent_id_idx
  ON ebxml.slot
  USING hash
  (parent_id COLLATE pg_catalog."default");
  
CREATE INDEX action_eventtype_idx
  ON ebxml.action
  USING hash
  (eventtype COLLATE pg_catalog."default");
  
CREATE INDEX auditableevent_timestamp_idx
  ON ebxml.auditableevent
  USING btree
  ("timestamp");
  
CREATE INDEX action_id_hash_idx
  ON ebxml.action
  USING hash
  (id);

CREATE INDEX deliveryinfo_id_hash_idx
  ON ebxml.deliveryinfo
  USING hash
  (id);

CREATE INDEX emailaddress_id_hash_idx
  ON ebxml.emailaddress
  USING hash
  (id);

CREATE INDEX registryobject_id_hash_idx
  ON ebxml.registryobject
  USING hash
  (id);

CREATE INDEX association_id_hash_idx
  ON ebxml.association
  USING hash
  (id);

CREATE INDEX auditableevent_id_hash_idx
  ON ebxml.auditableevent
  USING hash
  (id);

CREATE INDEX classification_id_hash_idx
  ON ebxml.classification
  USING hash
  (id);

CREATE INDEX externalidentifier_id_hash_idx
  ON ebxml.externalidentifier
  USING hash
  (id);


CREATE INDEX externallink_id_hash_idx
  ON ebxml.externallink
  USING hash
  (id);

CREATE INDEX extrinsicobject_id_hash_idx
  ON ebxml.extrinsicobject
  USING hash
  (id);

CREATE INDEX comment_id_hash_idx
  ON ebxml.comment
  USING hash
  (id);

CREATE INDEX federation_id_hash_idx
  ON ebxml.federation
  USING hash
  (id);


CREATE INDEX notification_id_hash_idx
  ON ebxml.notification
  USING hash
  (id);

CREATE INDEX organization_id_hash_idx
  ON ebxml.organization
  USING hash
  (id);

CREATE INDEX person_id_hash_idx
  ON ebxml.person
  USING hash
  (id);

CREATE INDEX querydefinition_id_hash_idx
  ON ebxml.querydefinition
  USING hash
  (id);


CREATE INDEX registrypackage_id_hash_idx
  ON ebxml.registrypackage
  USING hash
  (id);

CREATE INDEX registry_id_hash_idx
  ON ebxml.registry
  USING hash
  (id);

CREATE INDEX role_id_hash_idx
  ON ebxml.role
  USING hash
  (id);

CREATE INDEX servicebinding_id_hash_idx
  ON ebxml.servicebinding
  USING hash
  (id);


CREATE INDEX serviceendpoint_id_hash_idx
  ON ebxml.serviceendpoint
  USING hash
  (id);

CREATE INDEX serviceinterface_id_hash_idx
  ON ebxml.serviceinterface
  USING hash
  (id);

CREATE INDEX service_id_hash_idx
  ON ebxml.service
  USING hash
  (id);

CREATE INDEX subscription_id_hash_idx
  ON ebxml.subscription
  USING hash
  (id);


CREATE INDEX classificationnode_id_hash_idx
  ON ebxml.classificationnode
  USING hash
  (id);

CREATE INDEX classificationscheme_id_hash_idx
  ON ebxml.classificationscheme
  USING hash
  (id);

CREATE INDEX workflowaction_id_hash_idx
  ON ebxml.workflowaction
  USING hash
  (id);

CREATE INDEX parameter_id_hash_idx
  ON ebxml.parameter
  USING hash
  (id);


CREATE INDEX personname_id_hash_idx
  ON ebxml.personname
  USING hash
  (id);

CREATE INDEX postaladdress_id_hash_idx
  ON ebxml.postaladdress
  USING hash
  (id);

CREATE INDEX queryexpression_id_hash_idx
  ON ebxml.queryexpression
  USING hash
  (id);

CREATE INDEX stringqueryexpression_id_hash_idx
  ON ebxml.stringqueryexpression
  USING hash
  (id);


CREATE INDEX xmlqueryexpression_id_hash_idx
  ON ebxml.xmlqueryexpression
  USING hash
  (id);

CREATE INDEX query_id_hash_idx
  ON ebxml.query
  USING hash
  (id);

CREATE INDEX slot_id_hash_idx
  ON ebxml.slot
  USING hash
  (id);

CREATE INDEX telephonenumber_id_hash_idx
  ON ebxml.telephonenumber
  USING hash
  (id);


  