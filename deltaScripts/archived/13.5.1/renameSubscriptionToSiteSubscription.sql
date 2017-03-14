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

-- New classification nodes for SiteSubscription
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.SiteSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.SiteSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'com.raytheon.uf.common.datadelivery.registry.SiteSubscription', parent, '/com.raytheon.uf.common.datadelivery.registry.SiteSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:SiteSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:SiteSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'SiteSubscription', parent, '/urn:oasis:names:tc:ebxml-regrep:classificationScheme:ObjectType/RegistryObject/SiteSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Subscription';

-- Update foreign keys for the taxonomy to SiteSubscription
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.SiteSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:SiteSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Subscription';

-- Delete Subscription classification nodes
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Subscription';
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';

-- Update registry object references from Subscription to SiteSubscription
update ebxml.registryobjectlist_registryobject set registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.SiteSubscription' where registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
update ebxml.registryobject set objecttype = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.SiteSubscription' where objecttype ='urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.Subscription';
update ebxml.value set stringvalue = regexp_replace(stringvalue, '<subscription ', '<siteSubscription ', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '</subscription>', '</siteSubscription>', 'g');

-- Update the aggregated events
update events.aggregate set grouping = regexp_replace(grouping, 'com\.raytheon\.uf\.common\.datadelivery\.registry\.Subscription', 'com.raytheon.uf.common.datadelivery.registry.SiteSubscription', 'g');

-- New classification nodes for InitialPendingSiteSubscription
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription', parent, '/com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
insert into ebxml.classificationnode(id, lid, objecttype, owner, status, userversionname, versionname, description_key, name_key, code, parent, path) SELECT 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSiteSubscription', 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSiteSubscription', objecttype, owner, status, userversionname, versionname, description_key, name_key, 'SiteSubscription', parent, '/urn:oasis:names:tc:ebxml-regrep:classificationScheme:ObjectType/RegistryObject/SiteSubscription' FROM ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSubscription';

-- Update foreign keys for the taxonomy to InitialPendingSiteSubscription
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
update ebxml.taxonomyelementtype_classificationnode set classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSiteSubscription' where classificationnode_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSubscription';

-- Delete InitialPendingSubscription classification nodes
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:InitialPendingSubscription';
delete from ebxml.classificationnode where id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';

-- Update registry object references from InitialPendingSubscription to InitialPendingSiteSubscription
update ebxml.registryobjectlist_registryobject set registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription' where registryobject_id = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
update ebxml.registryobject set objecttype = 'urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription' where objecttype ='urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription';
update ebxml.value set stringvalue = regexp_replace(stringvalue, '<pendingSubscription ', '<pendingSiteSubscription ', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '</pendingSubscription>', '</pendingSiteSubscription>', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '<initialPendingSubscription ', '<initialPendingSiteSubscription ', 'g');
update ebxml.value set stringvalue = regexp_replace(stringvalue, '</initialPendingSubscription>', '</initialPendingSiteSubscription>', 'g');

-- Update the aggregated events
update events.aggregate set grouping = regexp_replace(grouping, 'com\.raytheon\.uf\.common\.datadelivery\.registry\.InitialPendingSubscription', 'com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription', 'g');

-- Commit the transaction
END;
