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
\set ON_ERROR_STOP 1
\connect metadata;

-- Start a transaction
BEGIN;

-- New classification nodes for UserSubscription
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.UserSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.UserSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'com.raytheon.uf.common.datadelivery.registry.UserSubscription', parent, '/com.raytheon.uf.common.datadelivery.registry.UserSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:UserSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:UserSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'UserSubscription', parent, '/urn:oasis:names:tc:ebxml-regrep:classificationScheme:ObjectType/RegistryObject/UserSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Subscription';

-- Update foreign keys for the taxonomy to UserSubscription
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.UserSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:UserSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Subscription';

-- Delete Subscription classification nodes
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Subscription';
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';

-- Update registry object references from Subscription to UserSubscription
update ebxml.registryobjectlist_registryobject set registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.UserSubscription' where registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
update ebxml.registryobject set objecttype = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.UserSubscription' where objecttype ='urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
update ebxml.value set stringvalue = regexp_replace(stringvalue, '<subscription ', '<userSubscription ', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '</subscription>', '</userSubscription>', 'g');

-- Update the aggregated events
update events.aggregate set grouping = regexp_replace(grouping, 'com\.raytheon\.uf\.common\.datadelivery\.registry\.Subscription', 'com.raytheon.uf.common.datadelivery.registry.UserSubscription', 'g');

-- New classification nodes for InitialPendingUserSubscription
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription', parent, '/com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingUserSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingUserSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'UserSubscription', parent, '/urn:oasis:names:tc:ebxml-regrep:classificationScheme:ObjectType/RegistryObject/UserSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSubscription';

-- Update foreign keys for the taxonomy to InitialPendingUserSubscription
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingUserSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSubscription';

-- Delete InitialPendingSubscription classification nodes
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSubscription';
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';

-- Update registry object references from InitialPendingSubscription to InitialPendingUserSubscription
update ebxml.registryobjectlist_registryobject set registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription' where registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
update ebxml.registryobject set objecttype = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription' where objecttype ='urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
update ebxml.value set stringvalue = regexp_replace(stringvalue, '<pendingSubscription ', '<pendingUserSubscription ', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '</pendingSubscription>', '</pendingUserSubscription>', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '<initialPendingSubscription ', '<initialPendingUserSubscription ', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '</initialPendingSubscription>', '</initialPendingUserSubscription>', 'g');

-- Update the aggregated events
update events.aggregate set grouping = regexp_replace(grouping, 'com\.raytheon\.uf\.common\.datadelivery\.registry\.InitialPendingSubscription', 'com.raytheon.uf.common.datadelivery.registry.InitialPendingUserSubscription', 'g');

-- Commit the transaction
END;