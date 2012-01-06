<?xml version='1.0' encoding='UTF-8'?>
<?profile class='org.eclipse.equinox.internal.p2.engine.Profile' version='0.0.2'?>
<profile id='PlatformProfile' timestamp='1221176230726'>
  <properties size='7'>
    <property name='org.eclipse.equinox.p2.cache' value='/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse'/>
    <property name='org.eclipse.equinox.p2.flavor' value='tooling'/>
    <property name='org.eclipse.equinox.p2.roaming' value='true'/>
    <property name='org.eclipse.update.install.features' value='true'/>
    <property name='org.eclipse.equinox.p2.environments' value='osgi.ws=gtk,osgi.os=linux,osgi.arch=x86'/>
    <property name='org.eclipse.equinox.p2.installFolder' value='/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse'/>
    <property name='eclipse.touchpoint.launcherName' value='eclipse'/>
  </properties>
  <units size='153'>
    <unit id='org.eclipse.equinox.p2.extensionlocation' version='1.0.2.R34x_v20080825'>
      <update id='org.eclipse.equinox.p2.extensionlocation' range='[0.0.0,1.0.2.R34x_v20080825)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Extension Location Repository Support'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.extensionlocation' version='1.0.2.R34x_v20080825'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.extensionlocation' version='1.0.2.R34x_v20080825'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.extensionlocation' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='21'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='3.4.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.generator.features' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.touchpoint.eclipse' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.update' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.directorywatcher' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.generator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.2.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.extensionlocation' version='1.0.2.R34x_v20080825'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.extensionlocation.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.extensionlocation;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.metadata.generator.features,org.eclipse.equinox.internal.p2.touchpoint.eclipse,org.eclipse.equinox.internal.p2.update,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.directorywatcher,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.generator,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository,org.eclipse.osgi.service.datalocation;version=&quot;1.1.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.2.0&quot;,org.eclipse.osgi.util,org.osgi.framework&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.0.2.R34x_v20080825&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.extensionlocation;x-friends:=&quot;org.eclipse.equinox.p2.reconciler.dropins&quot;&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;3.4.0&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.platform.ide.launcher.gtk.linux.x86.eclipse' version='3.4.0.M20080911-1700' singleton='false'>
      <provides size='1'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.x86.eclipse' version='3.4.0.M20080911-1700'/>
      </provides>
      <filter>
        (&amp; (osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86))
      </filter>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='configure'>
            setLauncherName(name:eclipse)
          </instruction>
          <instruction key='unconfigure'>
            setLauncherName()
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.runtime.compatibility.registry' version='3.2.200.v20080610' singleton='false'>
      <update id='org.eclipse.core.runtime.compatibility.registry' range='[0.0.0,3.2.200.v20080610)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.fragmentName' value='Eclipse Registry Compatibility Fragment'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%fragmentName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime.compatibility.registry' version='3.2.200.v20080610'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.runtime.compatibility.registry' version='3.2.200.v20080610'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.equinox.registry' version='3.2.200.v20080610'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='[3.3.0,3.5.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.runtime.compatibility.registry' version='3.2.200.v20080610'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Eclipse-PatchFragment: true&#xA;Bundle-Localization: fragment&#xA;Bundle-Name: %fragmentName&#xA;Bundle-ClassPath: runtime_registry_compatibility.jar&#xA;Manifest-Version: 1.0&#xA;Fragment-Host: org.eclipse.equinox.registry;bundle-version=&quot;[3.3.0,3.5.0)&quot;&#xA;Bundle-SymbolicName: org.eclipse.core.runtime.compatibility.registry&#xA;Bundle-Version: 3.2.200.v20080610
          </instruction>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.engine' version='1.0.1.R34x_v20080827'>
      <update id='org.eclipse.equinox.p2.engine' range='[0.0.0,1.0.1.R34x_v20080827)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Engine'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.engine' version='1.0.1.R34x_v20080827'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.engine' version='1.0.1.R34x_v20080827'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.engine' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine.phases' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='27'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository.io' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.persistence' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.signedcontent' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.3'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.engine' version='1.0.1.R34x_v20080827'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.1.R34x_v20080827&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.xml.parsers,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.metadata,org.eclipse.equinox.internal.p2.metadata.repository,org.eclipse.equinox.internal.p2.metadata.repository.io,org.eclipse.equinox.internal.p2.persistence,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.signedcontent;version=&quot;1.0.0&quot;,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;,org.xml.sax&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.engine.EngineActivator&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry,org.eclipse.osgi&#xA;Eclipse-RegisterBuddy: org.eclipse.equinox.p2.metadata.repository&#xA;Export-Package: org.eclipse.equinox.internal.p2.engine;x-friends:=&quot;org.eclipse.equinox.p2.touchpoint.eclipse,org.eclipse.equinox.p2.touchpoint.natives&quot;,org.eclipse.equinox.internal.provisional.p2.engine; x-friends:=&quot;org.eclipse.equinox.p2.director.app,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.engine.phases;x-friends:=&quot;org.eclipse.equinox.p2.ui&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.engine;singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='tooling.osgi.bundle.default' version='1.0.0' singleton='false'>
      <hostRequirements size='1'>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='0.0.0' multiple='true' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='tooling.osgi.bundle.default' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='tooling' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='0.0.0' multiple='true' greedy='false'/>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            setStartLevel(startLevel:4);
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>

          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.contenttype' version='3.3.0.v20080604-1400'>
      <update id='org.eclipse.core.contenttype' range='[0.0.0,3.3.0.v20080604-1400)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Eclipse Content Mechanism'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.contenttype' version='3.3.0.v20080604-1400'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.contenttype' version='3.3.0.v20080604-1400'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.content' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.content' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.preferences' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.ext' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.contenttype' version='3.3.0.v20080604-1400'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.core.internal.content.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.contenttype; singleton:=true&#xA;Import-Package: javax.xml.parsers,org.eclipse.osgi.service.debug,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework,org.osgi.util.tracker,org.xml.sax,org.xml.sax.ext,org.xml.sax.helpers&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.3.0.v20080604-1400&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.internal.content;x-internal:=true,org.eclipse.core.runtime.content&#xA;Require-Bundle: org.eclipse.equinox.preferences;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.equinox.registry;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.apache.commons.logging' version='1.0.4.v20080605-1930' singleton='false'>
      <update id='org.apache.commons.logging' range='[0.0.0,1.0.4.v20080605-1930)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Apache Commons Logging Plug-in'/>
        <property name='df_LT.bundleProvider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%bundleProvider'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.apache.commons.logging' version='1.0.4.v20080605-1930'/>
        <provided namespace='osgi.bundle' name='org.apache.commons.logging' version='1.0.4.v20080605-1930'/>
        <provided namespace='java.package' name='org.apache.commons.logging' version='1.0.4'/>
        <provided namespace='java.package' name='org.apache.commons.logging.impl' version='1.0.4'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.apache.commons.logging' version='1.0.4.v20080605-1930'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %bundleName&#xA;Bundle-Version: 1.0.4.v20080605-1930&#xA;Export-Package: org.apache.commons.logging;version=&quot;1.0.4&quot;,org.apache.commons.logging.impl;version=&quot;1.0.4&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-SymbolicName: org.apache.commons.logging&#xA;Bundle-Localization: plugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %bundleProvider
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.commands' version='3.4.0.I20080509-2000' singleton='false'>
      <update id='org.eclipse.core.commands' range='[0.0.0,3.4.0.I20080509-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Commands'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='11'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.commands' version='3.4.0.I20080509-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.commands' version='3.4.0.I20080509-2000'/>
        <provided namespace='java.package' name='org.eclipse.core.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.commands.common' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.commands.contexts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.commands.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.commands.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.commands.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.commands.util' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.commands' version='3.4.0.I20080509-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %pluginName&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-ClassPath: .&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.core.commands&#xA;Bundle-Version: 3.4.0.I20080509-2000&#xA;Export-Package: org.eclipse.core.commands,org.eclipse.core.commands.common,org.eclipse.core.commands.contexts,org.eclipse.core.commands.operations,org.eclipse.core.commands.util,org.eclipse.core.internal.commands.operations;x-internal:=true,org.eclipse.core.internal.commands.util;x-internal:=true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.rcp.feature.jar' version='3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341'>
      <update id='org.eclipse.rcp.feature.jar' range='[0.0.0,3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341)' severity='0'/>
      <properties size='9'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='org.eclipse.update.feature.plugin' value='org.eclipse.rcp'/>
        <property name='df_LT.featureName' value='Eclipse RCP'/>
        <property name='df_LT.copyright' value='Copyright (c) 2000, 2007 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Rich Client Platform'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.rcp.feature.jar' version='3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='feature' version='1.0.0'/>
        <provided namespace='org.eclipse.update.feature' name='org.eclipse.rcp' version='3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <filter>
        (org.eclipse.update.install.features=true)
      </filter>
      <artifacts size='1'>
        <artifact classifier='org.eclipse.update.feature' id='org.eclipse.rcp' version='3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.eclipse.ui.editors' version='3.4.0.v20080603-2000'>
      <update id='org.eclipse.ui.editors' range='[0.0.0,3.4.0.v20080603-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Default Text Editor'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='10'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.editors' version='3.4.0.v20080603-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.editors' version='3.4.0.v20080603-2000'/>
        <provided namespace='java.package' name='org.eclipse.ui.editors.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.editors.text.templates' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.editors.quickdiff' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.editors.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filebuffers' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.editors' version='3.4.0.v20080603-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.ui.internal.editors.text.EditorsPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui.editors; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.0.v20080603-2000&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.editors.text,org.eclipse.ui.editors.text.templates,org.eclipse.ui.internal.editors.quickdiff;x-internal:=true,org.eclipse.ui.internal.editors.text;x-internal:=true,org.eclipse.ui.internal.texteditor;x-internal:=true,org.eclipse.ui.texteditor&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.ide;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui.workbench.texteditor;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.filebuffers;visibility:=reexport;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='tooling.org.eclipse.update.feature.default' version='1.0.0' singleton='false'>
      <hostRequirements size='1'>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='feature' range='0.0.0' optional='true' multiple='true' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='tooling.org.eclipse.update.feature.default' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='tooling' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='feature' range='0.0.0' optional='true' multiple='true' greedy='false'/>
      </requires>
      <filter>
        (org.eclipse.update.install.features=true)
      </filter>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='uninstall'>
            uninstallFeature(feature:${artifact},featureId:default,featureVersion:default)
          </instruction>
          <instruction key='install'>
            installFeature(feature:${artifact},featureId:default,featureVersion:default)
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ltk.ui.refactoring' version='3.4.1.r341_v20080716-0800'>
      <update id='org.eclipse.ltk.ui.refactoring' range='[0.0.0,3.4.1.r341_v20080716-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Refactoring UI'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='15'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ltk.ui.refactoring' version='3.4.1.r341_v20080716-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ltk.ui.refactoring' version='3.4.1.r341_v20080716-0800'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.ui.refactoring' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.ui.refactoring.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.ui.refactoring.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.ui.refactoring.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.ui.refactoring.scripting' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.ui.refactoring.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.ui.refactoring' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.ui.refactoring.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.ui.refactoring.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.ui.refactoring.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.ui.refactoring.resource' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='13'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filebuffers' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ltk.core.refactoring' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.navigator' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.compare' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.team.core' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.team.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ltk.ui.refactoring' version='3.4.1.r341_v20080716-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.ltk.internal.ui.refactoring.RefactoringUIPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ltk.ui.refactoring; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.r341_v20080716-0800&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ltk.internal.ui.refactoring;x-internal:=true,org.eclipse.ltk.internal.ui.refactoring.actions;x-internal:=true,org.eclipse.ltk.internal.ui.refactoring.history;x-internal:=true,org.eclipse.ltk.internal.ui.refactoring.model;x-internal:=true,org.eclipse.ltk.internal.ui.refactoring.scripting;x-internal:=true,org.eclipse.ltk.internal.ui.refactoring.util;x-internal:=true,org.eclipse.ltk.ui.refactoring,org.eclipse.ltk.ui.refactoring.actions,org.eclipse.ltk.ui.refactoring.history,org.eclipse.ltk.ui.refactoring.model,org.eclipse.ltk.ui.refactoring.resource&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.filebuffers;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ltk.core.refactoring;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.navigator;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.compare;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.team.core;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.team.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.intro' version='3.2.201.v20080702_34x'>
      <update id='org.eclipse.ui.intro' range='[0.0.0,3.2.201.v20080702_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin_name' value='Welcome Framework'/>
        <property name='df_LT.provider_name' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%provider_name'/>
      </properties>
      <provides size='16'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.intro' version='3.2.201.v20080702_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.intro' version='3.2.201.v20080702_34x'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.html' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.model.loader' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.model.url' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.model.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.model.viewer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.parts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.presentations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.swt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.impl.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.intro.config' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help.base' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.transform' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.transform.dom' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.transform.stream' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.intro' version='3.2.201.v20080702_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %plugin_name&#xA;Bundle-Version: 3.2.201.v20080702_34x&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.xml.parsers,javax.xml.transform,javax.xml.transform.dom,javax.xml.transform.stream,org.w3c.dom,org.xml.sax&#xA;Bundle-Activator: org.eclipse.ui.internal.intro.impl.IntroPlugin&#xA;Bundle-Vendor: %provider_name&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.help.base;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.ui.internal.intro.impl;x-friends:=&quot;org.eclipse.ui.intro.universal,org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.intro.impl.html;x-internal:=true,org.eclipse.ui.internal.intro.impl.model;x-friends:=&quot;org.eclipse.ua.tests,org.eclipse.ui.intro.universal&quot;,org.eclipse.ui.internal.intro.impl.model.loader;x-friends:=&quot;org.eclipse.ua.tests,org.eclipse.ui.intro.universal&quot;,org.eclipse.ui.internal.intro.impl.model.url;x-internal:=true,org.eclipse.ui.internal.intro.impl.model.util;x-internal:=true,org.eclipse.ui.internal.intro.impl.model.viewer;x-internal:=true,org.eclipse.ui.internal.intro.impl.parts;x-internal:=true,org.eclipse.ui.internal.intro.impl.presentations;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.intro.impl.swt;x-internal:=true,org.eclipse.ui.internal.intro.impl.util;x-internal:=true,org.eclipse.ui.intro.config&#xA;Bundle-SymbolicName: org.eclipse.ui.intro; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.simpleconfigurator' version='1.0.0.v20080604'>
      <update id='org.eclipse.equinox.simpleconfigurator' range='[0.0.0,1.0.0.v20080604)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Simple Configurator'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.simpleconfigurator' version='1.0.0.v20080604'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.simpleconfigurator' version='1.0.0.v20080604'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.configurator' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='java.package' name='org.eclipse.osgi.framework.console' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.2.0' optional='true'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.service.startlevel' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.simpleconfigurator' version='1.0.0.v20080604'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.provisional.configurator;x-friends:=&quot;org.eclipse.equinox.p2.reconciler.dropins,org.eclipse.equinox.p2.console,org.eclipse.equinox.p2.ui&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Activator: org.eclipse.equinox.internal.simpleconfigurator.Activator&#xA;Bundle-Name: %bundleName&#xA;Bundle-Version: 1.0.0.v20080604&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.simpleconfigurator;singleton:=true&#xA;Import-Package: org.eclipse.osgi.framework.console;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.osgi.service.resolver;version=&quot;1.2.0&quot;;resolution:=optional,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.service.startlevel;version=&quot;1.0.0&quot;,org.osgi.util.tracker;version=&quot;1.3.0&quot;&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.apache.jasper' version='5.5.17.v200806031609' singleton='false'>
      <update id='org.apache.jasper' range='[0.0.0,5.5.17.v200806031609)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Apache Jasper 2 Plug-in'/>
        <property name='df_LT.bundleProvider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%bundleProvider'/>
      </properties>
      <provides size='15'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.apache.jasper' version='5.5.17.v200806031609'/>
        <provided namespace='osgi.bundle' name='org.apache.jasper' version='5.5.17.v200806031609'/>
        <provided namespace='java.package' name='org.apache.jasper' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.compiler' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.compiler.tagplugin' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.resources' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.runtime' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.security' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.servlet' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.tagplugins.jstl' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.tagplugins.jstl.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.jasper.xmlparser' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='18'>
        <required namespace='java.package' name='javax.servlet' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.jsp' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.jsp.el' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.jsp.resources' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.jsp.tagext' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.resources' range='2.4.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.apache.commons.el' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.apache.commons.logging' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.apache.tools.ant' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.apache.tools.ant.taskdefs' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.apache.tools.ant.types' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.apache.tools.ant.util' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.ext' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.apache.jasper' version='5.5.17.v200806031609'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %bundleProvider&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %bundleName&#xA;Import-Package: javax.servlet;version=&quot;2.4&quot;,javax.servlet.http;version=&quot;2.4&quot;,javax.servlet.jsp;version=&quot;2.0&quot;,javax.servlet.jsp.el;version=&quot;2.0&quot;,javax.servlet.jsp.resources;version=&quot;2.0&quot;,javax.servlet.jsp.tagext;version=&quot;2.0&quot;,javax.servlet.resources;version=&quot;2.4&quot;,javax.xml.parsers,org.apache.commons.el;version=&quot;[1.0.0,2.0.0)&quot;,org.apache.commons.logging;version=&quot;[1.0.0,2.0.0)&quot;,org.apache.tools.ant;resolution:=optional,org.apache.tools.ant.taskdefs;resolution:=optional,org.apache.tools.ant.types;resolution:=optional,org.apache.tools.ant.util;resolution:=optional,org.w3c.dom,org.xml.sax,org.xml.sax.ext,org.xml.sax.helpers&#xA;Bundle-ClassPath: .&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.apache.jasper&#xA;Bundle-Version: 5.5.17.v200806031609&#xA;Export-Package: org.apache.jasper,org.apache.jasper.compiler,org.apache.jasper.compiler.tagplugin,org.apache.jasper.resources,org.apache.jasper.runtime,org.apache.jasper.security,org.apache.jasper.servlet,org.apache.jasper.tagplugins.jstl,org.apache.jasper.tagplugins.jstl.core,org.apache.jasper.util,org.apache.jasper.xmlparser
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.metadata.repository' version='1.0.0.v20080604'>
      <update id='org.eclipse.equinox.p2.metadata.repository' range='[0.0.0,1.0.0.v20080604)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Metadata Repository'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.metadata.repository' version='1.0.0.v20080604'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.metadata.repository' version='1.0.0.v20080604'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository.io' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='26'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf' range='1.2.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf.filetransfer' range='2.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='3.4.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.jobs' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.preferences' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.app' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.persistence' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.security.storage' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.prefs' range='1.1.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.metadata.repository' version='1.0.0.v20080604'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.0.v20080604&#xA;Eclipse-LazyStart: true&#xA;Import-Package: org.eclipse.core.runtime.preferences,org.eclipse.equinox.app;resolution:=optional,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.metadata,org.eclipse.equinox.internal.p2.persistence,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.equinox.security.storage,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.prefs;version=&quot;1.1.0&quot;,org.xml.sax&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.metadata.repository.Activator&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry,org.eclipse.ecf;bundle-version=&quot;1.2.0&quot;,org.eclipse.ecf.filetransfer;bundle-version=&quot;2.0.0&quot;,org.eclipse.osgi;bundle-version=&quot;3.4.0&quot;,org.eclipse.core.jobs;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.equinox.internal.p2.metadata.repository;x-friends:=&quot;org.eclipse.equinox.p2.engine,org.eclipse.equinox.p2.metadata.generator,org.eclipse.equinox.p2.reconciler.dropins, org.eclipse.equinox.p2.ui&quot;,org.eclipse.equinox.internal.p2.metadata.repository.io;x-friends:=&quot;org.eclipse.equinox.p2.engine&quot;,org.eclipse.equinox.internal.provisional.p2.metadata.repository; x-friends:=&quot;org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository;x-friends:=&quot;org.eclipse.equinox.p2.extensionlocation,org.eclipse.equinox.p2.updatesite&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.metadata.repository;singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-BuddyPolicy: registered&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolingorg.eclipse.platform.ide.config.gtk.linux.x86' version='3.4.0.M20080911-1700'>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.config.gtk.linux.x86' version='3.4.0.M20080911-1700'/>
        <provided namespace='toolingorg.eclipse.platform.ide' name='org.eclipse.platform.ide.config' version='3.4.0.M20080911-1700'/>
      </provides>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='configure'>
            setProgramProperty(propName:osgi.ws, propValue:gtk);setProgramProperty(propName:osgi.instance.area.default, propValue:@user.home/workspace);setProgramProperty(propName:osgi.splashPath, propValue:platform:/base/plugins/org.eclipse.platform);setProgramProperty(propName:eclipse.buildId, propValue:M20080911-1700);setProgramProperty(propName:eclipse.product, propValue:org.eclipse.platform.ide);
          </instruction>
          <instruction key='unconfigure'>
            setProgramProperty(propName:osgi.ws, propValue:);setProgramProperty(propName:osgi.instance.area.default, propValue:);setProgramProperty(propName:osgi.splashPath, propValue:);setProgramProperty(propName:eclipse.buildId, propValue:);setProgramProperty(propName:eclipse.product, propValue:);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.garbagecollector' version='1.0.1.R34x_v20080818'>
      <update id='org.eclipse.equinox.p2.garbagecollector' range='[0.0.0,1.0.1.R34x_v20080818)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Provisioning Garbage Collector'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.garbagecollector' version='1.0.1.R34x_v20080818'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.garbagecollector' version='1.0.1.R34x_v20080818'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.garbagecollector' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='14'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.engine' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.preferences' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.4.0'/>
        <required namespace='java.package' name='org.osgi.service.prefs' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.3'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.garbagecollector' version='1.0.1.R34x_v20080818'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.garbagecollector.GCActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.garbagecollector;singleton:=true&#xA;Import-Package: org.eclipse.core.runtime.preferences,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.4.0&quot;,org.osgi.service.prefs;version=&quot;1.1.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.1.R34x_v20080818&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.garbagecollector;x-friends:=&quot;org.eclipse.equinox.p2.touchpoint.eclipse&quot;&#xA;Require-Bundle: org.eclipse.equinox.p2.engine,org.eclipse.osgi;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.equinox.registry;bundle-version=&quot;[3.4.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.metadata' version='1.0.0.v20080514-1900'>
      <update id='org.eclipse.equinox.p2.metadata' range='[0.0.0,1.0.0.v20080514-1900)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Metadata'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.metadata' version='1.0.0.v20080514-1900'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.metadata' version='1.0.0.v20080514-1900'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.metadata' version='1.0.0.v20080514-1900'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.metadata.MetadataActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.metadata;singleton:=true&#xA;Import-Package: org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.0.v20080514-1900&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.metadata; x-friends:=&quot;org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.metadata; x-friends:=&quot;org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.core,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.director.app,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.exemplarysetup,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.jarprocessor,  org.eclipse.equinox.p2.metadata,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.metadata.query; x-friends:=&quot;org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.core,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.director.app,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.exemplarysetup,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.jarprocessor,  org.eclipse.equinox.p2.metadata,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.console&quot;&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.p2.core
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolingorg.eclipse.equinox.p2.reconciler.dropins' version='1.0.2.R34x_v20080909' singleton='false'>
      <hostRequirements size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.reconciler.dropins' range='1.0.2.R34x_v20080909'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.equinox.p2.reconciler.dropins' version='1.0.2.R34x_v20080909'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='tooling' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.reconciler.dropins' range='1.0.2.R34x_v20080909'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            markStarted(started: true);mkdir(path:${installFolder}/dropins)
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>
            markStarted(started: false);rmdir(path:${installFolder}/dropins)
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.preferences' version='3.2.201.R34x_v20080709'>
      <update id='org.eclipse.equinox.preferences' range='[0.0.0,3.2.201.R34x_v20080709)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Eclipse Preferences Mechanism'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.preferences' version='3.2.201.R34x_v20080709'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.preferences' version='3.2.201.R34x_v20080709'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.preferences.exchange' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.preferences' version='3.2.0'/>
        <provided namespace='java.package' name='org.osgi.service.prefs' version='1.1.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='10'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.preferences' version='3.2.201.R34x_v20080709'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-ActivationPolicy: lazy; exclude:=&quot;org.eclipse.core.internal.preferences.exchange&quot;&#xA;Bundle-Name: %pluginName&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.equinox.registry;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional&#xA;Bundle-Activator: org.eclipse.core.internal.preferences.Activator&#xA;Bundle-Vendor: %providerName&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 3.2.201.R34x_v20080709&#xA;Bundle-Localization: plugin&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Export-Package: org.eclipse.core.internal.preferences;x-friends:=&quot;org.eclipse.core.resources,org.eclipse.core.runtime&quot;,org.eclipse.core.internal.preferences.exchange;x-friends:=&quot;org.eclipse.core.runtime&quot;,org.eclipse.core.runtime.preferences;version=&quot;3.2.0&quot;,org.osgi.service.prefs;version=&quot;1.1&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.preferences; singleton:=true&#xA;Import-Package: org.eclipse.osgi.framework.log,org.eclipse.osgi.service.datalocation,org.eclipse.osgi.service.debug,org.eclipse.osgi.service.environment,org.eclipse.osgi.util,org.osgi.framework,org.osgi.service.packageadmin,org.osgi.util.tracker&#xA;Eclipse-LazyStart: true; exceptions=&quot;org.eclipse.core.internal.preferences.exchange&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.jface.text' version='3.4.1.r341_v20080827-1100' singleton='false'>
      <update id='org.eclipse.jface.text' range='[0.0.0,3.4.1.r341_v20080827-1100)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JFace Text'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='27'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jface.text' version='3.4.1.r341_v20080827-1100'/>
        <provided namespace='osgi.bundle' name='org.eclipse.jface.text' version='3.4.1.r341_v20080827-1100'/>
        <provided namespace='java.package' name='org.eclipse.jface.contentassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.contentassist.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.text.html' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.text.link.contentassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.text.revisions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.text.source' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.contentassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.formatter' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.hyperlink' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.information' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.link' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.presentation' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.quickassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.reconciler' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.revisions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.rules' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.source' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.source.projection' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.source.projection.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.templates' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.templates.persistence' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.text' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.jface.text' version='3.4.1.r341_v20080827-1100'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %pluginName&#xA;Import-Package: com.ibm.icu.text&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.jface;bundle-version=&quot;[3.4.0,4.0.0)&quot;&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.jface.text&#xA;Bundle-Version: 3.4.1.r341_v20080827-1100&#xA;Export-Package: org.eclipse.jface.contentassist,org.eclipse.jface.contentassist.images,org.eclipse.jface.internal.text;x-internal:=true,org.eclipse.jface.internal.text.html;x-friends:=&quot;org.eclipse.ui.workbench.texteditor, org.eclipse.ui.editors, org.eclipse.jdt.debug.ui, org.eclipse.jdt.ui, org.eclipse.ant.ui, org.eclipse.ltk.ui.refactoring&quot;,org.eclipse.jface.internal.text.link.contentassist;x-internal:=true,org.eclipse.jface.internal.text.revisions;x-internal:=true,org.eclipse.jface.internal.text.source;x-internal:=true,org.eclipse.jface.text,org.eclipse.jface.text.contentassist,org.eclipse.jface.text.formatter,org.eclipse.jface.text.hyperlink,org.eclipse.jface.text.information,org.eclipse.jface.text.link,org.eclipse.jface.text.presentation,org.eclipse.jface.text.quickassist,org.eclipse.jface.text.reconciler,org.eclipse.jface.text.revisions,org.eclipse.jface.text.rules,org.eclipse.jface.text.source,org.eclipse.jface.text.source.projection,org.eclipse.jface.text.source.projection.images,org.eclipse.jface.text.templates,org.eclipse.jface.text.templates.persistence
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.cheatsheets' version='3.3.101.v20080702_34x'>
      <update id='org.eclipse.ui.cheatsheets' range='[0.0.0,3.3.101.v20080702_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.PROVIDER_NAME' value='Eclipse.org'/>
        <property name='df_LT.PLUGIN_NAME' value='Cheat Sheets'/>
        <property name='org.eclipse.equinox.p2.name' value='%PLUGIN_NAME'/>
        <property name='org.eclipse.equinox.p2.provider' value='%PROVIDER_NAME'/>
      </properties>
      <provides size='18'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.cheatsheets' version='3.3.101.v20080702_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.cheatsheets' version='3.3.101.v20080702_34x'/>
        <provided namespace='java.package' name='org.eclipse.ui.cheatsheets' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.composite.explorer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.composite.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.composite.parser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.composite.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.data' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.handlers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.registry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.state' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.cheatsheets.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.provisional.cheatsheets' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help.base' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.help.ui' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.apache.lucene.document' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.cheatsheets' version='3.3.101.v20080702_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %PLUGIN_NAME&#xA;Bundle-Version: 3.3.101.v20080702_34x&#xA;Eclipse-LazyStart: true&#xA;Import-Package: com.ibm.icu.text,javax.xml.parsers,org.apache.lucene.document,org.w3c.dom,org.xml.sax&#xA;Bundle-Activator: org.eclipse.ui.internal.cheatsheets.CheatSheetPlugin&#xA;Bundle-Vendor: %PROVIDER_NAME&#xA;Require-Bundle: org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.help.base;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.help.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional&#xA;Export-Package: org.eclipse.ui.cheatsheets,org.eclipse.ui.internal.cheatsheets;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.actions;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.composite.explorer;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.composite.model;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.composite.parser;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.composite.views;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.data;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.dialogs;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.handlers;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.registry;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.state;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.cheatsheets.views;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.provisional.cheatsheets;x-friends:=&quot;org.eclipse.ua.tests&quot;&#xA;Bundle-SymbolicName: org.eclipse.ui.cheatsheets; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ecf.filetransfer' version='2.0.0.v20080611-1715'>
      <update id='org.eclipse.ecf.filetransfer' range='[0.0.0,2.0.0.v20080611-1715)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin.name' value='ECF Filetransfer API'/>
        <property name='df_LT.plugin.provider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%plugin.provider'/>
      </properties>
      <provides size='9'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.filetransfer' version='2.0.0.v20080611-1715'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ecf.filetransfer' version='2.0.0.v20080611-1715'/>
        <provided namespace='java.package' name='org.eclipse.ecf.filetransfer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.filetransfer.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.filetransfer.identity' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.filetransfer.service' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.internal.filetransfer' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.jobs' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.log' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.url' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.2'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ecf.filetransfer' version='2.0.0.v20080611-1715'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Import-Package: org.eclipse.core.runtime.jobs,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.log;version=&quot;1.3.0&quot;,org.osgi.service.url;version=&quot;1.0.0&quot;,org.osgi.util.tracker;version=&quot;1.3.2&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Created-By: 10.0-b19 (Sun Microsystems Inc.)&#xA;Manifest-Version: 1.0&#xA;Bundle-Name: %plugin.name&#xA;Bundle-Vendor: %plugin.provider&#xA;Bundle-ActivationPolicy: lazy&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Bundle-Version: 2.0.0.v20080611-1715&#xA;Export-Package: org.eclipse.ecf.filetransfer,org.eclipse.ecf.filetransfer.events,org.eclipse.ecf.filetransfer.identity,org.eclipse.ecf.filetransfer.service,org.eclipse.ecf.internal.filetransfer;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry,org.eclipse.ecf&#xA;Bundle-Activator: org.eclipse.ecf.internal.filetransfer.Activator&#xA;Bundle-SymbolicName: org.eclipse.ecf.filetransfer;singleton:=true&#xA;Eclipse-LazyStart: true&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help.appserver' version='3.1.300.v20080507'>
      <update id='org.eclipse.help.appserver' range='[0.0.0,3.1.300.v20080507)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.appserver_plugin_name' value='Help Application Server'/>
        <property name='org.eclipse.equinox.p2.name' value='%appserver_plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.appserver' version='3.1.300.v20080507'/>
        <provided namespace='osgi.bundle' name='org.eclipse.help.appserver' version='3.1.300.v20080507'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.appserver' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.help.appserver' version='3.1.300.v20080507'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %appserver_plugin_name&#xA;Bundle-Activator: org.eclipse.help.internal.appserver.AppserverPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ClassPath: .&#xA;Bundle-SymbolicName: org.eclipse.help.appserver; singleton:=true&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.1.300.v20080507&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.help.internal.appserver;x-friends:=&quot;org.eclipse.help.base,org.eclipse.help.ui,org.eclipse.ua.tests,org.eclipse.tomcat&quot;&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolingorg.eclipse.equinox.simpleconfigurator' version='1.0.0.v20080604' singleton='false'>
      <hostRequirements size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.simpleconfigurator' range='1.0.0.v20080604'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.equinox.simpleconfigurator' version='1.0.0.v20080604'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='tooling' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.simpleconfigurator' range='1.0.0.v20080604'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            setStartLevel(startLevel:1);markStarted(started: true);
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>
            setStartLevel(startLevel:-1);markStarted(started: false);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help.feature.group' version='1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat' singleton='false'>
      <update id='org.eclipse.help.feature.group' range='[0.0.0,1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat)' severity='0'/>
      <properties size='9'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='org.eclipse.equinox.p2.type.group' value='true'/>
        <property name='df_LT.featureName' value='Eclipse Help System'/>
        <property name='df_LT.copyright' value='Copyright (c) 2008 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Eclipse help system.'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.feature.group' version='1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='25'>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.rcp.feature.group' range='[3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341,3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='javax.servlet' range='[2.4.0.v200806031604,2.4.0.v200806031604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='javax.servlet.jsp' range='[2.0.0.v200806031607,2.0.0.v200806031607]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.apache.ant' range='[1.7.0.v200803061910,1.7.0.v200803061910]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.apache.commons.el' range='[1.0.0.v200806031608,1.0.0.v200806031608]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.apache.commons.logging' range='[1.0.4.v20080605-1930,1.0.4.v20080605-1930]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.apache.jasper' range='[5.5.17.v200806031609,5.5.17.v200806031609]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.apache.lucene' range='[1.9.1.v20080530-1600,1.9.1.v20080530-1600]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.apache.lucene.analysis' range='[1.9.1.v20080530-1600,1.9.1.v20080530-1600]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.http.jetty' range='[1.1.0.v20080425,1.1.0.v20080425]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.http.registry' range='[1.0.100.v20080427-0830,1.0.100.v20080427-0830]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.http.servlet' range='[1.0.100.v20080427-0830,1.0.100.v20080427-0830]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.jsp.jasper' range='[1.0.100.v20080427-0830,1.0.100.v20080427-0830]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.jsp.jasper.registry' range='[1.0.0.v20080427-0830,1.0.0.v20080427-0830]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.base' range='[3.3.101.M20080728_34x,3.3.101.M20080728_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.ui' range='[3.3.101.M20080715_34x,3.3.101.M20080715_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.webapp' range='[3.3.101.M20080805_34x,3.3.101.M20080805_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.mortbay.jetty' range='[5.1.14.v200806031611,5.1.14.v200806031611]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.forms' range='[3.3.101.v20080708_34x,3.3.101.v20080708_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.osgi.services' range='[3.1.200.v20071203,3.1.200.v20071203]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.variables' range='[3.2.100.v20080529-1300,3.2.100.v20080529-1300]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ant.core' range='[3.2.0.v20080529,3.2.0.v20080529]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime.compatibility' range='[3.2.0.v20071008,3.2.0.v20071008]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime.compatibility.registry' range='[3.2.200.v20080610,3.2.200.v20080610]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.feature.jar' range='[1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat,1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat]'>
          <filter>
            (org.eclipse.update.install.features=true)
          </filter>
        </required>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.eclipse.ui.ide.application' version='1.0.0.I20080603-2000'>
      <update id='org.eclipse.ui.ide.application' range='[0.0.0,1.0.0.I20080603-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Eclipse IDE UI Application'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.ide.application' version='1.0.0.I20080603-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.ide.application' version='1.0.0.I20080603-2000'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.application' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.application.dialogs' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='[3.1.100,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.navigator.resources' range='[3.2.100,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.net' range='[1.0.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.core' range='[3.1.100,4.0.0)'/>
        <required namespace='osgi.bundle' name='com.ibm.icu' range='3.8.1'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.ide.application' version='1.0.0.I20080603-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Require-Bundle: org.eclipse.ui.ide;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.update.configurator;bundle-version=&quot;[3.1.100,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.navigator.resources;bundle-version=&quot;[3.2.100,4.0.0)&quot;,org.eclipse.core.net;bundle-version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.update.core;bundle-version=&quot;[3.1.100,4.0.0)&quot;,com.ibm.icu;bundle-version=&quot;3.8.1&quot;&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 1.0.0.I20080603-2000&#xA;Export-Package: org.eclipse.ui.internal.ide.application;x-internal:=true,org.eclipse.ui.internal.ide.application.dialogs;x-internal:=true&#xA;Bundle-SymbolicName: org.eclipse.ui.ide.application;singleton:=true&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %Plugin.name&#xA;Bundle-Vendor: %Plugin.providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help.ui' version='3.3.101.M20080715_34x'>
      <update id='org.eclipse.help.ui' range='[0.0.0,3.3.101.M20080715_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.help_system_plugin_name' value='Help System UI'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%help_system_plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='12'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.ui' version='3.3.101.M20080715_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.help.ui' version='3.3.101.M20080715_34x'/>
        <provided namespace='java.package' name='org.eclipse.help.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.internal.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.internal.browser.embedded' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.internal.search' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.internal.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.ui.internal.views' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.help.base' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.help.ui' version='3.3.101.M20080715_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %help_system_plugin_name&#xA;Bundle-Version: 3.3.101.M20080715_34x&#xA;Eclipse-LazyStart: true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-Activator: org.eclipse.help.ui.internal.HelpUIPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.help.base;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.ui.forms;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.help.ui,org.eclipse.help.ui.browser,org.eclipse.help.ui.internal;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.ui.internal.browser;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.ui.internal.browser.embedded;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.ui.internal.search;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.ui.internal.util;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.ui.internal.views;x-friends:=&quot;org.eclipse.ui.cheatsheets,org.eclipse.ua.tests&quot;&#xA;Bundle-SymbolicName: org.eclipse.help.ui; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='a.jre' version='1.6.0' singleton='false'>
      <provides size='117'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='a.jre' version='1.6.0'/>
        <provided namespace='java.package' name='javax.accessibility' version='0.0.0'/>
        <provided namespace='java.package' name='javax.activity' version='0.0.0'/>
        <provided namespace='java.package' name='javax.crypto' version='0.0.0'/>
        <provided namespace='java.package' name='javax.crypto.interfaces' version='0.0.0'/>
        <provided namespace='java.package' name='javax.crypto.spec' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio.event' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio.metadata' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio.plugins.bmp' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio.plugins.jpeg' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio.spi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.imageio.stream' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.loading' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.modelmbean' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.monitor' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.openmbean' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.relation' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.remote' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.remote.rmi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.management.timer' version='0.0.0'/>
        <provided namespace='java.package' name='javax.naming' version='0.0.0'/>
        <provided namespace='java.package' name='javax.naming.directory' version='0.0.0'/>
        <provided namespace='java.package' name='javax.naming.event' version='0.0.0'/>
        <provided namespace='java.package' name='javax.naming.ldap' version='0.0.0'/>
        <provided namespace='java.package' name='javax.naming.spi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.net' version='0.0.0'/>
        <provided namespace='java.package' name='javax.net.ssl' version='0.0.0'/>
        <provided namespace='java.package' name='javax.print' version='0.0.0'/>
        <provided namespace='java.package' name='javax.print.attribute' version='0.0.0'/>
        <provided namespace='java.package' name='javax.print.attribute.standard' version='0.0.0'/>
        <provided namespace='java.package' name='javax.print.event' version='0.0.0'/>
        <provided namespace='java.package' name='javax.rmi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.rmi.CORBA' version='0.0.0'/>
        <provided namespace='java.package' name='javax.rmi.ssl' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.auth' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.auth.callback' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.auth.kerberos' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.auth.login' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.auth.spi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.auth.x500' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.cert' version='0.0.0'/>
        <provided namespace='java.package' name='javax.security.sasl' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sound.midi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sound.midi.spi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sound.sampled' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sound.sampled.spi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sql' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sql.rowset' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sql.rowset.serial' version='0.0.0'/>
        <provided namespace='java.package' name='javax.sql.rowset.spi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.border' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.colorchooser' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.event' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.filechooser' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.plaf' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.plaf.basic' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.plaf.metal' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.plaf.multi' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.plaf.synth' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.table' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.text' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.text.html' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.text.html.parser' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.text.rtf' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.tree' version='0.0.0'/>
        <provided namespace='java.package' name='javax.swing.undo' version='0.0.0'/>
        <provided namespace='java.package' name='javax.transaction' version='0.0.0'/>
        <provided namespace='java.package' name='javax.transaction.xa' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.datatype' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.namespace' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.parsers' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.transform' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.transform.dom' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.transform.sax' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.transform.stream' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.validation' version='0.0.0'/>
        <provided namespace='java.package' name='javax.xml.xpath' version='0.0.0'/>
        <provided namespace='java.package' name='org.ietf.jgss' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA_2_3' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA_2_3.portable' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA.DynAnyPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA.ORBPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA.portable' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CORBA.TypeCodePackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CosNaming' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CosNaming.NamingContextExtPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.CosNaming.NamingContextPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.Dynamic' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.DynamicAny' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.DynamicAny.DynAnyFactoryPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.DynamicAny.DynAnyPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.IOP' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.IOP.CodecFactoryPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.IOP.CodecPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.Messaging' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableInterceptor' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableInterceptor.ORBInitInfoPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableServer' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableServer.CurrentPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableServer.POAManagerPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableServer.POAPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableServer.portable' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.PortableServer.ServantLocatorPackage' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.SendingContext' version='0.0.0'/>
        <provided namespace='java.package' name='org.omg.stub.java.rmi' version='0.0.0'/>
        <provided namespace='java.package' name='org.w3c.dom' version='0.0.0'/>
        <provided namespace='java.package' name='org.w3c.dom.bootstrap' version='0.0.0'/>
        <provided namespace='java.package' name='org.w3c.dom.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.w3c.dom.ls' version='0.0.0'/>
        <provided namespace='java.package' name='org.xml.sax' version='0.0.0'/>
        <provided namespace='java.package' name='org.xml.sax.ext' version='0.0.0'/>
        <provided namespace='java.package' name='org.xml.sax.helpers' version='0.0.0'/>
      </provides>
      <touchpoint id='org.eclipse.equinox.p2.native' version='1.0.0'/>
    </unit>
    <unit id='org.eclipse.equinox.jsp.jasper' version='1.0.100.v20080427-0830' singleton='false'>
      <update id='org.eclipse.equinox.jsp.jasper' range='[0.0.0,1.0.100.v20080427-0830)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Jasper Jsp Support Bundle'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.jsp.jasper' version='1.0.100.v20080427-0830'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.jsp.jasper' version='1.0.100.v20080427-0830'/>
        <provided namespace='java.package' name='org.eclipse.equinox.jsp.jasper' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='java.package' name='javax.servlet' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.jsp' range='2.0.0'/>
        <required namespace='java.package' name='org.apache.jasper.servlet' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.http' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.1'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.jsp.jasper' version='1.0.100.v20080427-0830'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %bundleName&#xA;Bundle-Version: 1.0.100.v20080427-0830&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.servlet;version=&quot;2.4&quot;,javax.servlet.http;version=&quot;2.4&quot;,javax.servlet.jsp;version=&quot;2.0&quot;,org.apache.jasper.servlet,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.http;version=&quot;1.2.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.util.tracker;version=&quot;1.3.1&quot;&#xA;Bundle-Activator: org.eclipse.equinox.internal.jsp.jasper.Activator&#xA;Bundle-Vendor: %providerName&#xA;Export-Package: org.eclipse.equinox.jsp.jasper;version=&quot;1.0.0&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.jsp.jasper&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.jarprocessor' version='1.0.0.v20080514-1900'>
      <update id='org.eclipse.equinox.p2.jarprocessor' range='[0.0.0,1.0.0.v20080514-1900)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning JAR Processor'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.jarprocessor' version='1.0.0.v20080514-1900'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.jarprocessor' version='1.0.0.v20080514-1900'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.jarprocessor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.jarprocessor.verifier' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.internal.provisional.equinox.p2.jarprocessor' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.jarprocessor' version='1.0.0.v20080514-1900'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.0.v20080514-1900&#xA;Export-Package: org.eclipse.equinox.internal.p2.jarprocessor;x-friends:=&quot;org.eclipse.equinox.p2.artifact.repository&quot;,org.eclipse.equinox.internal.p2.jarprocessor.verifier;x-internal:=true,org.eclipse.internal.provisional.equinox.p2.jarprocessor;x-friends:=&quot;org.eclipse.equinox.p2.artifact.optimizers,org.eclipse.equinox.p2.artifact.repository&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.jarprocessor;singleton:=true&#xA;Bundle-Localization: plugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.update.core' version='3.2.201.R34x_v20080714'>
      <update id='org.eclipse.update.core' range='[0.0.0,3.2.201.R34x_v20080714)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Install/Update Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='20'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.core' version='3.2.201.R34x_v20080714'/>
        <provided namespace='osgi.bundle' name='org.eclipse.update.core' version='3.2.201.R34x_v20080714'/>
        <provided namespace='java.package' name='org.eclipse.update.configuration' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.core.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.core.connection' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.jarprocessor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.mirror' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.provisional' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.search' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.security' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.verifier' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.search' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.standalone' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.net' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.update.core' version='3.2.201.R34x_v20080714'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.update.internal.core.UpdateCore&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.update.core; singleton:=true&#xA;Import-Package: javax.xml.parsers,org.w3c.dom,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.2.201.R34x_v20080714&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.update.configuration,org.eclipse.update.core,org.eclipse.update.core.model,org.eclipse.update.internal.core;x-friends:=&quot;org.eclipse.update.ui,org.eclipse.update.scheduler,org.eclipse.pde.build&quot;,org.eclipse.update.internal.core.connection;x-internal:=true,org.eclipse.update.internal.jarprocessor;x-friends:=&quot;org.eclipse.pde.build&quot;,org.eclipse.update.internal.mirror;x-internal:=true,org.eclipse.update.internal.model;x-friends:=&quot;org.eclipse.update.ui&quot;,org.eclipse.update.internal.operations;x-friends:=&quot;org.eclipse.update.ui,org.eclipse.update.scheduler&quot;,org.eclipse.update.internal.provisional;x-internal:=true,org.eclipse.update.internal.search;x-friends:=&quot;org.eclipse.update.ui&quot;,org.eclipse.update.internal.security;x-internal:=true,org.eclipse.update.internal.verifier;x-internal:=true,org.eclipse.update.operations,org.eclipse.update.search,org.eclipse.update.standalone&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.update.configurator;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.core.net;bundle-version=&quot;[1.0.0,2.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.sat4j.core' version='2.0.0.v20080602' singleton='false'>
      <update id='org.sat4j.core' range='[0.0.0,2.0.0.v20080602)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='SAT4J Core'/>
        <property name='df_LT.providerName' value='CRIL CNRS UMR 8188 - Universite d&apos;Artois'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='19'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.sat4j.core' version='2.0.0.v20080602'/>
        <provided namespace='osgi.bundle' name='org.sat4j.core' version='2.0.0.v20080602'/>
        <provided namespace='java.package' name='org.sat4j' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.constraints' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.constraints.card' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.constraints.cnf' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.learning' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.orders' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.restarts' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.minisat.uip' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.opt' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.reader' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.specs' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.tools' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.sat4j.core' version='2.0.0.v20080602'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %bundleName&#xA;Implementation-Version: 2.0&#xA;Export-Package: org.sat4j,org.sat4j.core,org.sat4j.minisat,org.sat4j.minisat.constraints,org.sat4j.minisat.constraints.card,org.sat4j.minisat.constraints.cnf,org.sat4j.minisat.core,org.sat4j.minisat.learning,org.sat4j.minisat.orders,org.sat4j.minisat.restarts,org.sat4j.minisat.uip,org.sat4j.opt,org.sat4j.reader,org.sat4j.specs,org.sat4j.tools&#xA;Created-By: 10.0-b22 (Sun Microsystems Inc.)&#xA;Specification-Title: SAT4J&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Manifest-Version: 1.0&#xA;Main-Class: org.sat4j.BasicLauncher&#xA;Bundle-Version: 2.0.0.v20080602&#xA;Implementation-Vendor: CRIL CNRS UMR 8188 - Universite d&apos;Artois&#xA;Implementation-Title: SAT4J&#xA;Built-By: Daniel Le Berre&#xA;Specification-Version: NA&#xA;Bundle-Localization: plugin&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-SymbolicName: org.sat4j.core&#xA;Specification-Vendor: Daniel Le Berre
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.filesystem.linux.x86' version='1.2.0.v20080604-1400'>
      <update id='org.eclipse.core.filesystem.linux.x86' range='[0.0.0,1.2.0.v20080604-1400)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.fragmentName' value='Core File System for Linux'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%fragmentName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.linux.x86' version='1.2.0.v20080604-1400'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.filesystem.linux.x86' version='1.2.0.v20080604-1400'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.core.filesystem' version='1.2.0.v20080604-1400'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
      </requires>
      <filter>
        (&amp; (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.filesystem.linux.x86' version='1.2.0.v20080604-1400'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %fragmentName&#xA;Bundle-Version: 1.2.0.v20080604-1400&#xA;Fragment-Host: org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;&#xA;Eclipse-PlatformFilter: (&amp; (osgi.os=linux) (osgi.arch=x86))&#xA;Bundle-SymbolicName: org.eclipse.core.filesystem.linux.x86; singleton:=true&#xA;Bundle-Localization: fragment&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.browser' version='3.2.201.v20080708_34x'>
      <update id='org.eclipse.ui.browser' range='[0.0.0,3.2.201.v20080708_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Browser Support'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.browser' version='3.2.201.v20080708_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.browser' version='3.2.201.v20080708_34x'/>
        <provided namespace='java.package' name='org.eclipse.ui.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.browser.browsers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.browser.macosx' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.browser' version='3.2.201.v20080708_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %Plugin.name&#xA;Bundle-Version: 3.2.201.v20080708_34x&#xA;Eclipse-LazyStart: true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-Activator: org.eclipse.ui.internal.browser.WebBrowserUIPlugin&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.ui.browser,org.eclipse.ui.internal.browser;x-internal:=true,org.eclipse.ui.internal.browser.browsers;x-internal:=true,org.eclipse.ui.internal.browser.macosx;x-internal:=true&#xA;Bundle-SymbolicName: org.eclipse.ui.browser; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.update.core.linux' version='3.2.0.v20080422' singleton='false'>
      <update id='org.eclipse.update.core.linux' range='[0.0.0,3.2.0.v20080422)' severity='0'/>
      <properties size='2'>
        <property name='org.eclipse.equinox.p2.name' value='%fragmentNameLinux'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.core.linux' version='3.2.0.v20080422'/>
        <provided namespace='osgi.bundle' name='org.eclipse.update.core.linux' version='3.2.0.v20080422'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.update.core' version='3.2.0.v20080422'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.update.core' range='[3.0.0,4.0.0)'/>
      </requires>
      <filter>
        (osgi.os=linux)
      </filter>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.update.core.linux' version='3.2.0.v20080422'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %fragmentNameLinux&#xA;Bundle-Version: 3.2.0.v20080422&#xA;Fragment-Host: org.eclipse.update.core;bundle-version=&quot;[3.0.0,4.0.0)&quot;&#xA;Eclipse-PlatformFilter: (osgi.os=linux)&#xA;Bundle-SymbolicName: org.eclipse.update.core.linux&#xA;Bundle-Localization: plugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.rcp' version='3.4.0.v20080507'>
      <update id='org.eclipse.rcp' range='[0.0.0,3.4.0.v20080507)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Eclipse RCP'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.rcp' version='3.4.0.v20080507'/>
        <provided namespace='osgi.bundle' name='org.eclipse.rcp' version='3.4.0.v20080507'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.rcp' version='3.4.0.v20080507'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Eclipse-AutoStart: true&#xA;Bundle-Version: 3.4.0.v20080507&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.rcp; singleton:=true&#xA;Bundle-Name: %pluginName&#xA;Bundle-Localization: plugin&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.console' version='1.0.0.v20080514-1900'>
      <update id='org.eclipse.equinox.p2.console' range='[0.0.0,1.0.0.v20080514-1900)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Console'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.console' version='1.0.0.v20080514-1900'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.console' version='1.0.0.v20080514-1900'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.console' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='17'>
        <required namespace='java.package' name='org.eclipse.core.runtime' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.configurator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.console' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.4.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.3'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.console' version='1.0.0.v20080514-1900'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.p2.console;x-friends:=&quot;org.eclipse.equinox.p2.director.app,org.eclipse.equinox.p2.tools&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.console.Activator&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.0.v20080514-1900&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.console;singleton:=true&#xA;Import-Package: org.eclipse.core.runtime;common=split,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.provisional.configurator,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.osgi.framework.console;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.osgi.service.environment;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.4.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ecf.identity' version='2.0.0.v20080611-1715'>
      <update id='org.eclipse.ecf.identity' range='[0.0.0,2.0.0.v20080611-1715)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin.name' value='ECF Identity API'/>
        <property name='df_LT.plugin.provider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%plugin.provider'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.identity' version='2.0.0.v20080611-1715'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ecf.identity' version='2.0.0.v20080611-1715'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.identity' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.internal.core.identity' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.log' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.2'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ecf.identity' version='2.0.0.v20080611-1715'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Import-Package: org.eclipse.osgi.service.debug;version=&quot;1.0.0&quot;,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.log;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.2&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Created-By: 10.0-b19 (Sun Microsystems Inc.)&#xA;Manifest-Version: 1.0&#xA;Bundle-Name: %plugin.name&#xA;Bundle-Vendor: %plugin.provider&#xA;Bundle-ActivationPolicy: lazy&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Bundle-Version: 2.0.0.v20080611-1715&#xA;Export-Package: org.eclipse.ecf.core.identity,org.eclipse.ecf.core.util,org.eclipse.ecf.internal.core.identity;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry&#xA;Bundle-Activator: org.eclipse.ecf.internal.core.identity.Activator&#xA;Bundle-SymbolicName: org.eclipse.ecf.identity;singleton:=true&#xA;Eclipse-LazyStart: true&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.jsch.core' version='1.1.100.I20080604'>
      <update id='org.eclipse.jsch.core' range='[0.0.0,1.1.100.I20080604)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JSch Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jsch.core' version='1.1.100.I20080604'/>
        <provided namespace='osgi.bundle' name='org.eclipse.jsch.core' version='1.1.100.I20080604'/>
        <provided namespace='java.package' name='org.eclipse.jsch.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jsch.internal.core' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='com.jcraft.jsch' range='[0.1.28,1.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.net' range='[1.0.0,2.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.jsch.core' version='1.1.100.I20080604'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.jsch.core,org.eclipse.jsch.internal.core;x-friends:=&quot;org.eclipse.jsch.ui&quot;&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,com.jcraft.jsch;bundle-version=&quot;[0.1.28,1.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.net;bundle-version=&quot;[1.0.0,2.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.jsch.internal.core.JSchCorePlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.1.100.I20080604&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.jsch.core;singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.jface' version='3.4.1.M20080827-2000' singleton='false'>
      <update id='org.eclipse.jface' range='[0.0.0,3.4.1.M20080827-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JFace'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='31'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jface' version='3.4.1.M20080827-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.jface' version='3.4.1.M20080827-2000'/>
        <provided namespace='java.package' name='org.eclipse.jface' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.action' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.action.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.bindings' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.bindings.keys' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.bindings.keys.formatting' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.contexts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.dialogs.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.fieldassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.fieldassist.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.provisional.action' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.layout' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.menus' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.operation' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.preference' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.preference.images' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.resource' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.viewers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.viewers.deferred' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.window' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.wizard' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.wizard.images' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.swt' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.commands' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.jface' version='3.4.1.M20080827-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 3.4.1.M20080827-2000&#xA;Import-Package: javax.xml.parsers,org.osgi.framework,org.w3c.dom,org.xml.sax&#xA;Bundle-Activator: org.eclipse.jface.internal.JFaceActivator&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.swt;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.commands;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.jface,org.eclipse.jface.action,org.eclipse.jface.action.images,org.eclipse.jface.bindings,org.eclipse.jface.bindings.keys,org.eclipse.jface.bindings.keys.formatting,org.eclipse.jface.commands,org.eclipse.jface.contexts,org.eclipse.jface.dialogs,org.eclipse.jface.dialogs.images,org.eclipse.jface.fieldassist,org.eclipse.jface.fieldassist.images,org.eclipse.jface.images,org.eclipse.jface.internal;x-friends:=&quot;org.eclipse.ui.workbench&quot;,org.eclipse.jface.internal.provisional.action;x-friends:=&quot;org.eclipse.ui.workbench&quot;,org.eclipse.jface.layout,org.eclipse.jface.menus,org.eclipse.jface.operation,org.eclipse.jface.preference,org.eclipse.jface.preference.images,org.eclipse.jface.resource,org.eclipse.jface.util,org.eclipse.jface.viewers,org.eclipse.jface.viewers.deferred,org.eclipse.jface.window,org.eclipse.jface.wizard,org.eclipse.jface.wizard.images&#xA;Bundle-SymbolicName: org.eclipse.jface&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.views.properties.tabbed' version='3.4.1.M20080730-0800'>
      <update id='org.eclipse.ui.views.properties.tabbed' range='[0.0.0,3.4.1.M20080730-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Tabbed Properties View'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.views.properties.tabbed' version='3.4.1.M20080730-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.views.properties.tabbed' version='3.4.1.M20080730-0800'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.properties.tabbed' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.properties.tabbed.l10n' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.properties.tabbed.view' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.properties.tabbed' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.views' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.views.properties.tabbed' version='3.4.1.M20080730-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %Plugin.name&#xA;Bundle-Activator: org.eclipse.ui.internal.views.properties.tabbed.TabbedPropertyViewPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui.views.properties.tabbed;singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.M20080730-0800&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.internal.views.properties.tabbed;x-internal:=true,org.eclipse.ui.internal.views.properties.tabbed.l10n;x-internal:=true,org.eclipse.ui.internal.views.properties.tabbed.view;x-friends:=&quot;org.eclipse.ui.tests.views.properties.tabbed&quot;,org.eclipse.ui.views.properties.tabbed&#xA;Require-Bundle: org.eclipse.ui.forms;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.ui.views;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.user.ui.feature.group' version='1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE' singleton='false'>
      <update id='org.eclipse.equinox.p2.user.ui.feature.group' range='[0.0.0,1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE)' severity='0'/>
      <properties size='9'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='org.eclipse.equinox.p2.type.group' value='true'/>
        <property name='df_LT.featureName' value='Equinox p2 Provisioning'/>
        <property name='df_LT.copyright' value='Copyright (c) 2008 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Eclipse Provisioning Platform'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.user.ui.feature.group' version='1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='35'>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.director' range='[1.0.2.v20080806-1619,1.0.2.v20080806-1619]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.core' range='[1.0.0.v20080530-1237,1.0.0.v20080530-1237]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.engine' range='[1.0.1.R34x_v20080827,1.0.1.R34x_v20080827]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.jarprocessor' range='[1.0.0.v20080514-1900,1.0.0.v20080514-1900]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.metadata' range='[1.0.0.v20080514-1900,1.0.0.v20080514-1900]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.metadata.repository' range='[1.0.0.v20080604,1.0.0.v20080604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.artifact.repository' range='[1.0.2.R34x_v20080808-1156,1.0.2.R34x_v20080808-1156]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf' range='[2.0.0.v20080611-1715,2.0.0.v20080611-1715]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.filetransfer' range='[2.0.0.v20080611-1715,2.0.0.v20080611-1715]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.identity' range='[2.0.0.v20080611-1715,2.0.0.v20080611-1715]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.exemplarysetup' range='[1.0.0.v20080427-2136,1.0.0.v20080427-2136]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.touchpoint.eclipse' range='[1.0.2.R34x_v20080910,1.0.2.R34x_v20080910]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.touchpoint.natives' range='[1.0.0.v20080505-1850,1.0.0.v20080505-1850]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.frameworkadmin' range='[1.0.2.R34x_v20080910,1.0.2.R34x_v20080910]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.frameworkadmin.equinox' range='[1.0.2.R34x_v20080911,1.0.2.R34x_v20080911]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.console' range='[1.0.0.v20080514-1900,1.0.0.v20080514-1900]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.provider.filetransfer' range='[2.0.0.v20080611-1715,2.0.0.v20080611-1715]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.simpleconfigurator.manipulator' range='[1.0.2.R34x_v20080911,1.0.2.R34x_v20080911]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.ui' range='[1.0.1.R34x_v20080909,1.0.1.R34x_v20080909]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.ui.sdk' range='[1.0.1.R34x_v20080818,1.0.1.R34x_v20080818]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.updatechecker' range='[1.0.0.v20080427-2136,1.0.0.v20080427-2136]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.garbagecollector' range='[1.0.1.R34x_v20080818,1.0.1.R34x_v20080818]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.directorywatcher' range='[1.0.2.v20080806-1619,1.0.2.v20080806-1619]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.reconciler.dropins' range='[1.0.2.R34x_v20080909,1.0.2.R34x_v20080909]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.metadata.generator' range='[1.0.1.R34x_v20080819,1.0.1.R34x_v20080819]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.updatesite' range='[1.0.1.R34x_v20080808-1156,1.0.1.R34x_v20080808-1156]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.sat4j.core' range='[2.0.0.v20080602,2.0.0.v20080602]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.sat4j.pb' range='[2.0.0.v20080602,2.0.0.v20080602]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security' range='[1.0.1.R34x_v20080721,1.0.1.R34x_v20080721]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security.ui' range='[1.0.0.v20080603-1810,1.0.0.v20080603-1810]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.extensionlocation' range='[1.0.2.R34x_v20080825,1.0.2.R34x_v20080825]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.provider.filetransfer.ssl' range='[1.0.0.v20080611-1715,1.0.0.v20080611-1715]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.ssl' range='[1.0.0.v20080611-1715,1.0.0.v20080611-1715]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.director.app' range='[1.0.1.R34x_v20080729,1.0.1.R34x_v20080729]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.user.ui.feature.jar' range='[1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE,1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE]'>
          <filter>
            (org.eclipse.update.install.features=true)
          </filter>
        </required>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.eclipse.update.configurator' version='3.2.201.R34x_v20080819'>
      <update id='org.eclipse.update.configurator' range='[0.0.0,3.2.201.R34x_v20080819)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Install/Update Configurator'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.configurator' version='3.2.201.R34x_v20080819'/>
        <provided namespace='osgi.bundle' name='org.eclipse.update.configurator' version='3.2.201.R34x_v20080819'/>
        <provided namespace='java.package' name='org.eclipse.update.configurator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.configurator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.configurator.branding' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='6'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.update.configurator' version='3.2.201.R34x_v20080819'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.update.internal.configurator.ConfigurationActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.update.configurator; singleton:=true&#xA;Import-Package: javax.xml.parsers,org.w3c.dom,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.2.201.R34x_v20080819&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.update.configurator,org.eclipse.update.internal.configurator;x-friends:=&quot;org.eclipse.update.core&quot;,org.eclipse.update.internal.configurator.branding;x-friends:=&quot;org.eclipse.update.core&quot;&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.osgi;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.metadata.generator' version='1.0.1.R34x_v20080819'>
      <update id='org.eclipse.equinox.p2.metadata.generator' range='[0.0.0,1.0.1.R34x_v20080819)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Metadata Generator'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.metadata.generator' version='1.0.1.R34x_v20080819'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.metadata.generator' version='1.0.1.R34x_v20080819'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.generator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.generator.features' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.generator' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='19'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.core' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.app' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.equinox' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.utils' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.frameworkadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository.processing' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.metadata.generator' version='1.0.1.R34x_v20080819'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.metadata.generator.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.metadata.generator;singleton:=true&#xA;Import-Package: javax.xml.parsers,org.eclipse.equinox.app;version=&quot;1.0.0&quot;,org.eclipse.equinox.internal.frameworkadmin.equinox,org.eclipse.equinox.internal.frameworkadmin.utils,org.eclipse.equinox.internal.p2.artifact.repository,org.eclipse.equinox.internal.p2.metadata,org.eclipse.equinox.internal.p2.metadata.repository,org.eclipse.equinox.internal.provisional.frameworkadmin,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.artifact.repository.processing,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.w3c.dom,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.0.1.R34x_v20080819&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.metadata.generator;x-internal:=true,org.eclipse.equinox.internal.p2.metadata.generator.features;x-friends:=&quot;org.eclipse.equinox.p2.updatesite,org.eclipse.equinox.p2.directorywatcher,org.eclipse.equinox.p2.extensionlocation&quot;,org.eclipse.equinox.internal.provisional.p2.metadata.generator; x-friends:=&quot;org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.updatesite&quot;&#xA;Require-Bundle: org.eclipse.equinox.p2.core,org.eclipse.osgi,org.eclipse.equinox.common
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.app' version='1.1.0.v20080421-2006'>
      <update id='org.eclipse.equinox.app' range='[0.0.0,1.1.0.v20080421-2006)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Application Container'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.app' version='1.1.0.v20080421-2006'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.app' version='1.1.0.v20080421-2006'/>
        <provided namespace='java.package' name='org.eclipse.equinox.app' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.app' version='0.0.0'/>
        <provided namespace='java.package' name='org.osgi.service.application' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='16'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='org.osgi.service.event' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.console' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.runnable' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.storagemanager' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.condpermadmin' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.osgi.service.event' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.app' version='1.1.0.v20080421-2006'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Version: 1.1.0.v20080421-2006&#xA;Bundle-ActivationPolicy: lazy&#xA;Eclipse-LazyStart: true&#xA;Export-Package: org.eclipse.equinox.app;version=&quot;1.0&quot;,org.eclipse.equinox.internal.app;x-friends:=&quot;org.eclipse.core.runtime&quot;,org.osgi.service.application;version=&quot;1.0&quot;&#xA;Import-Package: org.eclipse.osgi.framework.console;resolution:=optional,org.eclipse.osgi.framework.log,org.eclipse.osgi.service.datalocation,org.eclipse.osgi.service.debug,org.eclipse.osgi.service.environment; version=&quot;1.1&quot;,org.eclipse.osgi.service.runnable,org.eclipse.osgi.storagemanager,org.eclipse.osgi.util,org.osgi.framework;version=&quot;1.3&quot;,org.osgi.service.condpermadmin; resolution:=optional,org.osgi.service.event;version=&quot;1.0.0&quot;;resolution:=optional,org.osgi.service.packageadmin;version=&quot;1.2&quot;,org.osgi.util.tracker&#xA;Manifest-Version: 1.0&#xA;Bundle-Activator: org.eclipse.equinox.internal.app.Activator&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Bundle-RequiredExecutionEnvironment: OSGi/Minimum-1.1&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.registry;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.app; singleton:=true&#xA;Comment-DynamicImport: this is only used to allow late binding of the package&#xA;DynamicImport-Package: org.osgi.service.event;version=&quot;1.0.0&quot;&#xA;Bundle-Vendor: %providerName&#xA;Bundle-Name: %pluginName&#xA;Bundle-ManifestVersion: 2
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.director' version='1.0.2.v20080806-1619'>
      <update id='org.eclipse.equinox.p2.director' range='[0.0.0,1.0.2.v20080806-1619)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Director'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.director' version='1.0.2.v20080806-1619'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.director' version='1.0.2.v20080806-1619'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.director' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.resolution' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.rollback' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='17'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.sat4j.core' range='2.0.0'/>
        <required namespace='osgi.bundle' name='org.sat4j.pb' range='2.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.director' version='1.0.2.v20080806-1619'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.director.DirectorActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.director;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.2.v20080806-1619&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.director;x-friends:=&quot;org.eclipse.equinox.p2.exemplarysetup&quot;,org.eclipse.equinox.internal.p2.resolution;x-internal:=true,org.eclipse.equinox.internal.p2.rollback;x-friends:=&quot;org.eclipse.equinox.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.director; x-friends:=&quot;org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.director.app,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.pde.p2.ui&quot;&#xA;Require-Bundle: org.eclipse.equinox.common,org.sat4j.core;bundle-version=&quot;2.0.0&quot;,org.sat4j.pb;bundle-version=&quot;2.0.0&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.databinding.beans' version='1.1.1.M20080827-0800a' singleton='false'>
      <update id='org.eclipse.core.databinding.beans' range='[0.0.0,1.1.1.M20080827-0800a)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JFace Data Binding for JavaBeans'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.databinding.beans' version='1.1.1.M20080827-0800a'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.databinding.beans' version='1.1.1.M20080827-0800a'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.beans' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding.beans' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.core.databinding' range='[1.0.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.databinding.beans' version='1.1.1.M20080827-0800a'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %pluginName&#xA;Require-Bundle: org.eclipse.core.databinding;bundle-version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-ClassPath: .&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.core.databinding.beans&#xA;Bundle-Version: 1.1.1.M20080827-0800a&#xA;Export-Package: org.eclipse.core.databinding.beans,org.eclipse.core.internal.databinding.beans;x-internal:=true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.externaltools' version='3.2.0.v20080514-1542'>
      <update id='org.eclipse.ui.externaltools' range='[0.0.0,3.2.0.v20080514-1542)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='External Tools'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='11'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.externaltools' version='3.2.0.v20080514-1542'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.externaltools' version='3.2.0.v20080514-1542'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.launchConfigurations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.menu' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.program.launchConfigurations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.registry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.externaltools.internal.variables' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.variables' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.debug.core' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.debug.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.externaltools' version='3.2.0.v20080514-1542'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.ui.externaltools.internal.launchConfigurations;x-friends:=&quot;org.eclipse.ant.ui&quot;,org.eclipse.ui.externaltools.internal.menu;x-internal:=true,org.eclipse.ui.externaltools.internal.model;x-friends:=&quot;org.eclipse.ant.ui&quot;,org.eclipse.ui.externaltools.internal.program.launchConfigurations;x-friends:=&quot;org.eclipse.ant.ui&quot;,org.eclipse.ui.externaltools.internal.registry;x-internal:=true,org.eclipse.ui.externaltools.internal.ui;x-friends:=&quot;org.eclipse.ant.ui&quot;,org.eclipse.ui.externaltools.internal.variables;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.ui.ide;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.variables;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.debug.core;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.debug.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.ui.externaltools.internal.model.ExternalToolsPlugin&#xA;Bundle-Name: %Plugin.name&#xA;Bundle-Version: 3.2.0.v20080514-1542&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Bundle-SymbolicName: org.eclipse.ui.externaltools; singleton:=true&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.launcher.gtk.linux.x86' version='1.0.101.R34x_v20080805'>
      <update id='org.eclipse.equinox.launcher.gtk.linux.x86' range='[0.0.0,1.0.101.R34x_v20080805)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Launcher Linux X86 Fragment'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.x86' version='1.0.101.R34x_v20080805'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.launcher.gtk.linux.x86' version='1.0.101.R34x_v20080805'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.equinox.launcher' version='1.0.101.R34x_v20080805'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.launcher' range='[1.0.0,1.1.0)'/>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.launcher.gtk.linux.x86' version='1.0.101.R34x_v20080805'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.101.R34x_v20080805&#xA;Fragment-Host: org.eclipse.equinox.launcher;bundle-version=&quot;[1.0.0,1.1.0)&quot;&#xA;Eclipse-PlatformFilter: (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))&#xA;Bundle-SymbolicName: org.eclipse.equinox.launcher.gtk.linux.x86;singleton:=true&#xA;Bundle-Localization: launcher.gtk.linux.x86&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName
          </instruction>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.http.servlet' version='1.0.100.v20080427-0830' singleton='false'>
      <update id='org.eclipse.equinox.http.servlet' range='[0.0.0,1.0.100.v20080427-0830)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Http Services Servlet'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.http.servlet' version='1.0.100.v20080427-0830'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.http.servlet' version='1.0.100.v20080427-0830'/>
        <provided namespace='java.package' name='org.eclipse.equinox.http.servlet' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='java.package' name='javax.servlet' range='2.3.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.3.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.http' range='1.2.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.http.servlet' version='1.0.100.v20080427-0830'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %bundleName&#xA;Bundle-Version: 1.0.100.v20080427-0830&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.servlet;version=&quot;2.3&quot;,javax.servlet.http;version=&quot;2.3&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.http;version=&quot;1.2.0&quot;&#xA;Bundle-Activator: org.eclipse.equinox.http.servlet.internal.Activator&#xA;Bundle-Vendor: %providerName&#xA;Export-Package: org.eclipse.equinox.http.servlet;version=&quot;1.0.0&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.http.servlet&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.rcp.feature.group' version='3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341' singleton='false'>
      <update id='org.eclipse.rcp.feature.group' range='[0.0.0,3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341)' severity='0'/>
      <properties size='9'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='org.eclipse.equinox.p2.type.group' value='true'/>
        <property name='df_LT.featureName' value='Eclipse RCP'/>
        <property name='df_LT.copyright' value='Copyright (c) 2000, 2007 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Rich Client Platform'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.rcp.feature.group' version='3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='60'>
        <required namespace='org.eclipse.equinox.p2.iu' name='com.ibm.icu' range='[3.8.1.v20080530,3.8.1.v20080530]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.commands' range='[3.4.0.I20080509-2000,3.4.0.I20080509-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.databinding' range='[1.1.1.M20080827-0800b,1.1.1.M20080827-0800b]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.databinding.beans' range='[1.1.1.M20080827-0800a,1.1.1.M20080827-0800a]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.jobs' range='[3.4.0.v20080512,3.4.0.v20080512]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.expressions' range='[3.4.0.v20080603-2000,3.4.0.v20080603-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime.compatibility.auth' range='[3.2.100.v20070502,3.2.100.v20070502]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.osgi' range='[3.4.2.R34x_v20080826-1230,3.4.2.R34x_v20080826-1230]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help' range='[3.3.101.v20080702_34x,3.3.101.v20080702_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt' range='[3.4.1.v3449c,3.4.1.v3449c]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jface' range='[3.4.1.M20080827-2000,3.4.1.M20080827-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jface.databinding' range='[1.2.1.M20080827-0800a,1.2.1.M20080827-0800a]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui' range='[3.4.1.M20080910-0800,3.4.1.M20080910-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.workbench' range='[3.4.1.M20080827-0800a,3.4.1.M20080827-0800a]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.carbon' range='[4.0.0.I20080610-1200,4.0.0.I20080610-1200]'>
          <filter>
            (&amp;(osgi.os=macosx)(osgi.ws=carbon))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.simpleconfigurator' range='[1.0.0.v20080604,1.0.0.v20080604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.win32.win32.x86' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.win32.win32.x86_64' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=win32)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.wpf.win32.x86' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=wpf)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.win32.win32.ia64' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=ia64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.x86' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.s390' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=s390))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.s390x' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=s390x))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.solaris.sparc' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=solaris)(osgi.ws=gtk)(osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.solaris.x86' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=solaris)(osgi.ws=gtk)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.ppc' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.x86_64' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.carbon.macosx' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=macosx)(osgi.ws=carbon))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.motif.aix.ppc' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=aix)(osgi.ws=motif)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.motif.hpux.PA_RISC' range='[3.4.0.v3448f,3.4.0.v3448f]'>
          <filter>
            (&amp;(osgi.os=hpux)(osgi.ws=motif)(osgi.arch=PA_RISC))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.motif.hpux.ia64_32' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=hpux)(osgi.ws=motif)(osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.motif.linux.x86' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=motif)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.ia64' range='[3.4.0.HEAD,3.4.0.HEAD]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=ia64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.motif.solaris.sparc' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=solaris)(osgi.ws=motif)(osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.photon.qnx.x86' range='[3.4.1.v3449c,3.4.1.v3449c]'>
          <filter>
            (&amp;(osgi.os=qnx)(osgi.ws=photon)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.rcp' range='[3.4.0.v20080507,3.4.0.v20080507]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.contenttype' range='[3.3.0.v20080604-1400,3.3.0.v20080604-1400]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.app' range='[1.1.0.v20080421-2006,1.1.0.v20080421-2006]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.preferences' range='[3.2.201.R34x_v20080709,3.2.201.R34x_v20080709]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.registry' range='[3.4.0.v20080516-0950,3.4.0.v20080516-0950]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher' range='[1.0.101.R34x_v20080819,1.0.101.R34x_v20080819]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.solaris.sparc' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=solaris)(osgi.ws=gtk)(osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.carbon.macosx' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=macosx)(osgi.ws=carbon))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.ppc' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.x86' range='[1.0.101.R34x_v20080805,1.0.101.R34x_v20080805]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.s390' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=s390))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.s390x' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=s390x))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.motif.linux.x86' range='[1.0.101.R34x_v20080805,1.0.101.R34x_v20080805]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=motif)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.x86_64' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.motif.aix.ppc' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=aix)(osgi.ws=motif)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.motif.hpux.ia64_32' range='[1.0.1.R34x_v20080731,1.0.1.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=hpux)(osgi.ws=motif)(osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.motif.hpux.PA_RISC' range='[1.0.100.v20080303,1.0.100.v20080303]'>
          <filter>
            (&amp;(osgi.os=hpux)(osgi.ws=motif)(osgi.arch=PA_RISC))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.wpf.win32.x86' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=wpf)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.win32.win32.x86' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.win32.win32.x86_64' range='[1.0.101.R34x_v20080731,1.0.101.R34x_v20080731]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=win32)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.win32.win32.ia64' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.ws=win32)(osgi.arch=ia64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.rcp.feature.jar' range='[3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341,3.4.100.r341_v20080814-989JESIEdAciFYfkZZsBfSwQ2341]'>
          <filter>
            (org.eclipse.update.install.features=true)
          </filter>
        </required>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.eclipse.equinox.p2.director.app' version='1.0.1.R34x_v20080729'>
      <update id='org.eclipse.equinox.p2.director.app' range='[0.0.0,1.0.1.R34x_v20080729)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Director Application'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.director.app' version='1.0.1.R34x_v20080729'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.director.app' version='1.0.1.R34x_v20080729'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.director.app' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='18'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='3.4.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.app' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.console' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine.phases' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.director.app' version='1.0.1.R34x_v20080729'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.director.app.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.director.app;singleton:=true&#xA;Import-Package: org.eclipse.equinox.app,org.eclipse.equinox.internal.p2.console,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.engine,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.engine.phases,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.osgi.service.environment,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.osgi.framework&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.1.R34x_v20080729&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.director.app;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.osgi;bundle-version=&quot;3.4.0&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.jobs' version='3.4.0.v20080512'>
      <update id='org.eclipse.core.jobs' range='[0.0.0,3.4.0.v20080512)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Eclipse Jobs Mechanism'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.jobs' version='3.4.0.v20080512'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.jobs' version='3.4.0.v20080512'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.jobs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.jobs' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='6'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.jobs' version='3.4.0.v20080512'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.core.internal.jobs.JobActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.jobs; singleton:=true&#xA;Import-Package: org.eclipse.osgi.service.debug,org.eclipse.osgi.util,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin,org.osgi.util.tracker&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.4.0.v20080512&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.internal.jobs;x-internal:=true,org.eclipse.core.runtime.jobs&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.common' version='3.4.0.v20080421-2006'>
      <update id='org.eclipse.equinox.common' range='[0.0.0,3.4.0.v20080421-2006)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Common Eclipse Runtime'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.common' version='3.4.0.v20080421-2006'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.common' version='3.4.0.v20080421-2006'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.boot' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.runtime' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime' version='3.4.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='10'>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.localization' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.urlconversion' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.service.url' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.common' version='3.4.0.v20080421-2006'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-Version: 3.4.0.v20080421-2006&#xA;Eclipse-LazyStart: true&#xA;Import-Package: org.eclipse.osgi.framework.log,org.eclipse.osgi.service.datalocation,org.eclipse.osgi.service.debug,org.eclipse.osgi.service.localization,org.eclipse.osgi.service.urlconversion,org.osgi.service.url,org.eclipse.osgi.util,org.osgi.framework,org.osgi.service.packageadmin,org.osgi.util.tracker&#xA;Bundle-Activator: org.eclipse.core.internal.runtime.Activator&#xA;Bundle-Vendor: %providerName&#xA;Export-Package: org.eclipse.core.internal.boot;x-friends:=&quot;org.eclipse.core.resources,org.eclipse.core.runtime.compatibility,org.eclipse.pde.build&quot;,org.eclipse.core.internal.runtime;common=split;mandatory:=common; x-friends:=&quot;org.eclipse.core.contenttype,  org.eclipse.core.jobs,  org.eclipse.equinox.preferences,  org.eclipse.equinox.registry,  org.eclipse.core.runtime,  org.eclipse.core.runtime.compatibility,  org.eclipse.core.filesystem,  org.eclipse.equinox.security&quot;,org.eclipse.core.runtime;common=split;version=&quot;3.4.0&quot;;mandatory:=common&#xA;Bundle-SymbolicName: org.eclipse.equinox.common; singleton:=true&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ltk.core.refactoring' version='3.4.1.r341_v20080716-0800'>
      <update id='org.eclipse.ltk.core.refactoring' range='[0.0.0,3.4.1.r341_v20080716-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Refactoring Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='13'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ltk.core.refactoring' version='3.4.1.r341_v20080716-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ltk.core.refactoring' version='3.4.1.r341_v20080716-0800'/>
        <provided namespace='java.package' name='org.eclipse.ltk.core.refactoring' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.core.refactoring.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.core.refactoring.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.core.refactoring.participants' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.core.refactoring.resource' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.core.refactoring' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.core.refactoring.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.core.refactoring.resource' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ltk.internal.core.refactoring.resource.undostates' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filebuffers' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.commands' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.text' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ltk.core.refactoring' version='3.4.1.r341_v20080716-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.ltk.internal.core.refactoring.RefactoringCorePlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ltk.core.refactoring; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.r341_v20080716-0800&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ltk.core.refactoring,org.eclipse.ltk.core.refactoring.history,org.eclipse.ltk.core.refactoring.model,org.eclipse.ltk.core.refactoring.participants,org.eclipse.ltk.core.refactoring.resource,org.eclipse.ltk.internal.core.refactoring;x-friends:=&quot;org.eclipse.ltk.ui.refactoring,org.eclipse.ltk.core.refactoring.tests&quot;,org.eclipse.ltk.internal.core.refactoring.history;x-friends:=&quot;org.eclipse.ltk.ui.refactoring,org.eclipse.ltk.core.refactoring.tests&quot;,org.eclipse.ltk.internal.core.refactoring.resource;x-friends:=&quot;org.eclipse.ltk.ui.refactoring,org.eclipse.ltk.core.refactoring.tests&quot;,org.eclipse.ltk.internal.core.refactoring.resource.undostates;x-internal:=true&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;,org.eclipse.core.filebuffers;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.commands;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.platform.feature.jar' version='3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu'>
      <update id='org.eclipse.platform.feature.jar' range='[0.0.0,3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu)' severity='0'/>
      <properties size='8'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='df_LT.featureName' value='Eclipse Platform'/>
        <property name='df_LT.copyright' value='Copyright (c) 2000, 2007 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Common OS-independent base of the Eclipse platform. (Binary runtime and user documentation.)'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.feature.jar' version='3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='feature' version='1.0.0'/>
        <provided namespace='org.eclipse.update.feature' name='org.eclipse.platform' version='3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <filter>
        (org.eclipse.update.install.features=true)
      </filter>
      <artifacts size='1'>
        <artifact classifier='org.eclipse.update.feature' id='org.eclipse.platform' version='3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.eclipse.ui.workbench.compatibility' version='3.2.0.I20080509-2000' singleton='false'>
      <update id='org.eclipse.ui.workbench.compatibility' range='[0.0.0,3.2.0.I20080509-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.fragmentName' value='Workbench Compatibility'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%fragmentName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.workbench.compatibility' version='3.2.0.I20080509-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.workbench.compatibility' version='3.2.0.I20080509-2000'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.ui.workbench' version='3.2.0.I20080509-2000'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench' range='[3.0.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.workbench.compatibility' version='3.2.0.I20080509-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: fragment-compatibility&#xA;Bundle-Name: %fragmentName&#xA;Require-Bundle: org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-ClassPath: compatibility.jar&#xA;Manifest-Version: 1.0&#xA;Fragment-Host: org.eclipse.ui.workbench;bundle-version=&quot;[3.0.0,4.0.0)&quot;&#xA;Bundle-SymbolicName: org.eclipse.ui.workbench.compatibility&#xA;Bundle-Version: 3.2.0.I20080509-2000
          </instruction>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.artifact.repository' version='1.0.2.R34x_v20080808-1156'>
      <update id='org.eclipse.equinox.p2.artifact.repository' range='[0.0.0,1.0.2.R34x_v20080808-1156)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Artifact Repository Support'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='12'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.artifact.repository' version='1.0.2.R34x_v20080808-1156'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.artifact.repository' version='1.0.2.R34x_v20080808-1156'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.mirror' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.processing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.processors.pack200' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.repository' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.repository.simple' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository.processing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='28'>
        <required namespace='osgi.bundle' name='org.eclipse.ecf.filetransfer' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.jobs' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.preferences' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.equinox.app' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.jarprocessor' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.persistence' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.security.storage' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.internal.provisional.equinox.p2.jarprocessor' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.signedcontent' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.prefs' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.artifact.repository' version='1.0.2.R34x_v20080808-1156'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.2.R34x_v20080808-1156&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.xml.parsers,org.eclipse.core.runtime.jobs,org.eclipse.core.runtime.preferences;resolution:=optional,org.eclipse.equinox.app;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.jarprocessor,org.eclipse.equinox.internal.p2.metadata,org.eclipse.equinox.internal.p2.persistence,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.equinox.security.storage,org.eclipse.internal.provisional.equinox.p2.jarprocessor;resolution:=optional,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.signedcontent;version=&quot;1.0.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.prefs;version=&quot;1.1.0&quot;,org.osgi.util.tracker;version=&quot;1.3.0&quot;,org.w3c.dom,org.xml.sax;resolution:=optional&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.artifact.repository.Activator&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.ecf.filetransfer,org.eclipse.ecf,org.eclipse.equinox.common,org.eclipse.equinox.registry&#xA;Export-Package: org.eclipse.equinox.internal.p2.artifact.mirror;x-internal:=true,org.eclipse.equinox.internal.p2.artifact.processing;x-friends:=&quot;org.eclipse.equinox.p2.artifact.processors,org.eclipse.equinox.p2.artifact.optimizers&quot;,org.eclipse.equinox.internal.p2.artifact.processors.pack200;x-friends:=&quot;org.eclipse.equinox.p2.artifact.processors,org.eclipse.equinox.p2.artifact.optimizers&quot;,org.eclipse.equinox.internal.p2.artifact.repository;x-friends:=&quot;org.eclipse.equinox.p2.metadata.generator,org.eclipse.equinox.p2.reconciler.dropins&quot;,org.eclipse.equinox.internal.p2.artifact.repository.simple;x-friends:=&quot;org.eclipse.equinox.p2.selfhosting,org.eclipse.equinox.p2.touchpoint.eclipse,org.eclipse.equinox.p2.tests&quot;,org.eclipse.equinox.internal.provisional.p2.artifact.repository; x-friends:=&quot;org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.installer&quot;,org.eclipse.equinox.internal.provisional.p2.artifact.repository.processing; x-friends:=&quot;org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.ui&quot;,org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository; x-friends:=&quot;org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.updatesite&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.artifact.repository;singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-BuddyPolicy: registered&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.user.ui.feature.jar' version='1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE'>
      <update id='org.eclipse.equinox.p2.user.ui.feature.jar' range='[0.0.0,1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE)' severity='0'/>
      <properties size='8'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='df_LT.featureName' value='Equinox p2 Provisioning'/>
        <property name='df_LT.copyright' value='Copyright (c) 2008 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Eclipse Provisioning Platform'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.user.ui.feature.jar' version='1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='feature' version='1.0.0'/>
        <provided namespace='org.eclipse.update.feature' name='org.eclipse.equinox.p2.user.ui' version='1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <filter>
        (org.eclipse.update.install.features=true)
      </filter>
      <artifacts size='1'>
        <artifact classifier='org.eclipse.update.feature' id='org.eclipse.equinox.p2.user.ui' version='1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.eclipse.ant.core' version='3.2.0.v20080529'>
      <update id='org.eclipse.ant.core' range='[0.0.0,3.2.0.v20080529)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Ant Build Tool Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ant.core' version='3.2.0.v20080529'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ant.core' version='3.2.0.v20080529'/>
        <provided namespace='java.package' name='org.eclipse.ant.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ant.internal.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ant.internal.core.contentDescriber' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.core.variables' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ant.core' version='3.2.0.v20080529'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.ant.core,org.eclipse.ant.internal.core;x-friends:=&quot;org.eclipse.ant.ui&quot;,org.eclipse.ant.internal.core.contentDescriber;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.core.variables;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.ant.core.AntCorePlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.2.0.v20080529&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.ant.core; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true; exceptions=&quot;org.eclipse.ant.internal.core.contentDescriber&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.views' version='3.3.0.I20080509-2000'>
      <update id='org.eclipse.ui.views' range='[0.0.0,3.3.0.I20080509-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Views'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='9'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.views' version='3.3.0.I20080509-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.views' version='3.3.0.I20080509-2000'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.contentoutline' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.properties' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.contentoutline' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.properties' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.views' version='3.3.0.I20080509-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.ui.internal.views.ViewsPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ClassPath: .&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui.views; singleton:=true&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.3.0.I20080509-2000&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.internal.views;x-internal:=true,org.eclipse.ui.internal.views.contentoutline;x-internal:=true,org.eclipse.ui.internal.views.properties; ui.views=&quot;split&quot;; mandatory:=&quot;ui.views&quot;; x-internal:=true,org.eclipse.ui.views.contentoutline,org.eclipse.ui.views.properties; ui.views=&quot;split&quot;; mandatory:=&quot;ui.views&quot;&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.osgi.services' version='3.1.200.v20071203' singleton='false'>
      <update id='org.eclipse.osgi.services' range='[0.0.0,3.1.200.v20071203)' severity='0'/>
      <properties size='7'>
        <property name='df_LT.osgiServices' value='OSGi Release 4.0.1 Services'/>
        <property name='df_LT.osgiServicesDes' value='OSGi Service Platform Release 4.0.1 Service Interfaces and Classes'/>
        <property name='df_LT.eclipse.org' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%osgiServices'/>
        <property name='org.eclipse.equinox.p2.description' value='%osgiServicesDes'/>
        <property name='org.eclipse.equinox.p2.provider' value='%eclipse.org'/>
        <property name='org.eclipse.equinox.p2.contact' value='www.eclipse.org'/>
      </properties>
      <provides size='16'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.osgi.services' version='3.1.200.v20071203'/>
        <provided namespace='osgi.bundle' name='org.eclipse.osgi.services' version='3.1.200.v20071203'/>
        <provided namespace='java.package' name='org.osgi.service.cm' version='1.2.0'/>
        <provided namespace='java.package' name='org.osgi.service.component' version='1.0.0'/>
        <provided namespace='java.package' name='org.osgi.service.device' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.event' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.http' version='1.2.0'/>
        <provided namespace='java.package' name='org.osgi.service.io' version='1.0.0'/>
        <provided namespace='java.package' name='org.osgi.service.log' version='1.3.0'/>
        <provided namespace='java.package' name='org.osgi.service.metatype' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.provisioning' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.upnp' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.useradmin' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.wireadmin' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='java.package' name='javax.servlet' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='javax.servlet.http' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.2.0'/>
        <required namespace='java.package' name='javax.servlet' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='javax.servlet.http' range='0.0.0' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.osgi.services' version='3.1.200.v20071203'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Description: %osgiServicesDes&#xA;Bundle-RequiredExecutionEnvironment: OSGi/Minimum-1.0&#xA;Bundle-Name: %osgiServices&#xA;Bundle-ContactAddress: www.eclipse.org&#xA;Bundle-Vendor: %eclipse.org&#xA;Bundle-Copyright: %copyright&#xA;Bundle-DocUrl: http://www.eclipse.org&#xA;DynamicImport-Package: javax.servlet,javax.servlet.http&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 3.1.200.v20071203&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.osgi.service.cm; version=&quot;1.2&quot;,org.osgi.service.component; version=&quot;1.0&quot;,org.osgi.service.device; version=&quot;1.1&quot;,org.osgi.service.event; version=&quot;1.1&quot;,org.osgi.service.http; version=&quot;1.2&quot;,org.osgi.service.io; version=&quot;1.0&quot;,org.osgi.service.log; version=&quot;1.3&quot;,org.osgi.service.metatype; version=&quot;1.1&quot;,org.osgi.service.provisioning; version=&quot;1.1&quot;,org.osgi.service.upnp; version=&quot;1.1&quot;,org.osgi.service.useradmin; version=&quot;1.1&quot;,org.osgi.service.wireadmin; version=&quot;1.0&quot;&#xA;Bundle-SymbolicName: org.eclipse.osgi.services&#xA;Import-Package: org.osgi.framework; version=1.2,javax.servlet; resolution:=&quot;optional&quot;,javax.servlet.http; resolution:=&quot;optional&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.platform.ide.launcher.gtk.linux.x86' version='3.4.0.M20080911-1700'>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.x86' version='3.4.0.M20080911-1700'/>
        <provided namespace='toolingorg.eclipse.platform.ide' name='org.eclipse.platform.ide.launcher' version='3.4.0.M20080911-1700'/>
      </provides>
      <requires size='1'>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher.gtk.linux.x86' range='0.0.0'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
          </filter>
        </required>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <artifacts size='1'>
        <artifact classifier='binary' id='org.eclipse.platform.ide.launcher.gtk.linux.x86' version='3.4.0.M20080911-1700'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.native' version='1.0.0'/>
    </unit>
    <unit id='org.eclipse.platform.ide' version='3.4.0.M20080911-1700'>
      <update id='org.eclipse.platform.ide' range='0.0.0' severity='0'/>
      <properties size='3'>
        <property name='org.eclipse.equinox.p2.name' value='Eclipse Platform'/>
        <property name='lineUp' value='true'/>
        <property name='org.eclipse.equinox.p2.type.group' value='true'/>
      </properties>
      <provides size='1'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide' version='3.4.0.M20080911-1700'/>
      </provides>
      <requires size='67'>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingmotif.aix.ppcorg.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=aix) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.ppcorg.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.x86.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.carbon.macosx.x86' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwin32.win32.x86org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.motif.hpux.ia64_32' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=hpux) (osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.solaris.sparcorg.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=solaris) (osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingcarbon.macosx.ppcorg.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86_64org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.win32.win32.x86_64.eclipse.exe' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=win32)(osgi.ws=win32)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.ppcorg.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwin32.win32.x86_64org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingcarbon.macosx.ppcorg.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingmotif.hpux.ia64_32org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=hpux) (osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingcarbon.macosx.x86org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.carbon.macosx.x86.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=macosx)(osgi.ws=carbon)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwpf.win32.x86org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=wpf) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.equinox.p2.reconciler.dropins' range='[1.0.2.R34x_v20080909,1.0.2.R34x_v20080909]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.x86_64.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=linux)(osgi.ws=gtk)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.equinox.simpleconfigurator' range='[1.0.0.v20080604,1.0.0.v20080604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwpf.win32.x86org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=wpf) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwin32.win32.x86org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86_64org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.gtk.linux.x86_64' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.motif.hpux.ia64_32.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=hpux)(osgi.ws=motif)(osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.gtk.linux.ppc' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwin32.win32.x86_64org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.motif.aix.ppc' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=aix) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingmotif.hpux.ia64_32org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=hpux) (osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.user.ui.feature.group' range='[1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE,1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwpf.win32.x86org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=wpf) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.carbon.macosx.ppc' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingcarbon.macosx.x86org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.motif.aix.ppc.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=aix)(osgi.ws=motif)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingcarbon.macosx.ppcorg.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.equinox.launcher' range='[1.0.101.R34x_v20080819,1.0.101.R34x_v20080819]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingmotif.aix.ppcorg.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=aix) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwin32.win32.x86_64org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.win32.win32.x86_64' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingcarbon.macosx.x86org.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=carbon) (osgi.os=macosx) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.carbon.macosx.ppc.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=macosx)(osgi.ws=carbon)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.gtk.linux.x86' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.ppc.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=linux)(osgi.ws=gtk)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingwin32.win32.x86org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.solaris.sparc.eclipse' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=solaris)(osgi.ws=gtk)(osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.solaris.sparcorg.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=solaris) (osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.wpf.win32.x86' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=wpf) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.feature.group' range='[3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu,3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingmotif.aix.ppcorg.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=aix) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.gtk.solaris.sparc' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=solaris) (osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.solaris.sparcorg.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=solaris) (osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86_64org.eclipse.equinox.common' range='[3.4.0.v20080421-2006,3.4.0.v20080421-2006]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.wpf.win32.x86.eclipse.exe' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=win32)(osgi.ws=wpf)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingmotif.hpux.ia64_32org.eclipse.core.runtime' range='[3.4.0.v20080512,3.4.0.v20080512]'>
          <filter>
            (&amp; (osgi.ws=motif) (osgi.os=hpux) (osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.win32.win32.x86.eclipse.exe' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.os=win32)(osgi.ws=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.win32.win32.x86' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'>
          <filter>
            (&amp; (osgi.ws=win32) (osgi.os=win32) (osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher' range='[1.0.101.R34x_v20080819,1.0.101.R34x_v20080819]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.ppcorg.eclipse.update.configurator' range='[3.2.201.R34x_v20080819,3.2.201.R34x_v20080819]'>
          <filter>
            (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='toolingorg.eclipse.platform.ide' name='org.eclipse.platform.ide.launcher' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]' multiple='true'/>
        <required namespace='toolingorg.eclipse.platform.ide' name='org.eclipse.platform.ide.ini' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'/>
        <required namespace='toolingorg.eclipse.platform.ide' name='org.eclipse.platform.ide.config' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='tooling.osgi.bundle.default' range='0.0.0'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='tooling.source.default' range='0.0.0'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='tooling.org.eclipse.update.feature.default' range='0.0.0' optional='true'>
          <filter>
            (org.eclipse.update.install.features=true)
          </filter>
        </required>
      </requires>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='configure'>

          </instruction>
          <instruction key='unconfigure'>

          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.filesystem' version='1.2.0.v20080604-1400'>
      <update id='org.eclipse.core.filesystem' range='[0.0.0,1.2.0.v20080604-1400)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Core File Systems'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem' version='1.2.0.v20080604-1400'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.filesystem' version='1.2.0.v20080604-1400'/>
        <provided namespace='java.package' name='org.eclipse.core.filesystem' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.filesystem.provider' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.filesystem' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.filesystem.local' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.filesystem' version='1.2.0.v20080604-1400'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.core.filesystem,org.eclipse.core.filesystem.provider,org.eclipse.core.internal.filesystem;x-internal:=true,org.eclipse.core.internal.filesystem.local;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry,org.eclipse.osgi&#xA;Bundle-Activator: org.eclipse.core.internal.filesystem.Activator&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.2.0.v20080604-1400&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.filesystem; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.platform.feature.group' version='3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu' singleton='false'>
      <update id='org.eclipse.platform.feature.group' range='[0.0.0,3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu)' severity='0'/>
      <properties size='9'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='org.eclipse.equinox.p2.type.group' value='true'/>
        <property name='df_LT.featureName' value='Eclipse Platform'/>
        <property name='df_LT.copyright' value='Copyright (c) 2000, 2007 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Common OS-independent base of the Eclipse platform. (Binary runtime and user documentation.)'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.feature.group' version='3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='67'>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.feature.group' range='[1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat,1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.user.ui.feature.group' range='[1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE,1.0.1.r34x_v20080721-7d-7OEMsLAz-vTBJ-ZHQaF155CE]' optional='true'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='com.jcraft.jsch' range='[0.1.37.v200803061811,0.1.37.v200803061811]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.compare' range='[3.4.0.I20080604,3.4.0.I20080604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.boot' range='[3.1.100.v20080218,3.1.100.v20080218]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filebuffers' range='[3.4.0.v20080603-2000,3.4.0.v20080603-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem' range='[1.2.0.v20080604-1400,1.2.0.v20080604-1400]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.net' range='[1.1.0.I20080604,1.1.0.I20080604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.net.win32.x86' range='[1.0.0.I20080521,1.0.0.I20080521]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.net.linux.x86' range='[1.0.0.I20080521,1.0.0.I20080521]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.resources' range='[3.4.1.R34x_v20080902,3.4.1.R34x_v20080902]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.resources.compatibility' range='[3.4.0.v20080604-1400,3.4.0.v20080604-1400]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.osgi.util' range='[3.1.300.v20080303,3.1.300.v20080303]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.debug.core' range='[3.4.0.v20080612,3.4.0.v20080612]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.debug.ui' range='[3.4.1.v20080811_r341,3.4.1.v20080811_r341]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.appserver' range='[3.1.300.v20080507,3.1.300.v20080507]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ltk.core.refactoring' range='[3.4.1.r341_v20080716-0800,3.4.1.r341_v20080716-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ltk.ui.refactoring' range='[3.4.1.r341_v20080716-0800,3.4.1.r341_v20080716-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform' range='[3.3.101.v200809111700,3.3.101.v200809111700]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.doc.user' range='[3.4.1.r341_v20080808-0800,3.4.1.r341_v20080808-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.search' range='[3.4.1.r341_v20080813-0800,3.4.1.r341_v20080813-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.team.core' range='[3.4.1.r34x_20080827,3.4.1.r34x_20080827]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.team.ui' range='[3.4.1.r34x_20080827,3.4.1.r34x_20080827]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.text' range='[3.4.0.v20080605-1800,3.4.0.v20080605-1800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jface.text' range='[3.4.1.r341_v20080827-1100,3.4.1.r341_v20080827-1100]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jsch.core' range='[1.1.100.I20080604,1.1.100.I20080604]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jsch.ui' range='[1.1.100.I20080415,1.1.100.I20080415]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.console' range='[3.3.0.v20080529-1300,3.3.0.v20080529-1300]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.presentations.r21' range='[3.2.100.I20080512-2000,3.2.100.I20080512-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.intro' range='[3.2.201.v20080702_34x,3.2.201.v20080702_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.intro.universal' range='[3.2.200.v20080508,3.2.200.v20080508]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.cheatsheets' range='[3.3.101.v20080702_34x,3.3.101.v20080702_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.browser' range='[3.2.201.v20080708_34x,3.2.201.v20080708_34x]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.navigator' range='[3.3.101.M20080827-0800,3.3.101.M20080827-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.navigator.resources' range='[3.3.101.M20080910-0800,3.3.101.M20080910-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.net' range='[1.0.0.I20080605,1.0.0.I20080605]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.workbench.texteditor' range='[3.4.1.r341_v20080827-1100,3.4.1.r341_v20080827-1100]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.views' range='[3.3.0.I20080509-2000,3.3.0.I20080509-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.editors' range='[3.4.0.v20080603-2000,3.4.0.v20080603-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.externaltools' range='[3.2.0.v20080514-1542,3.2.0.v20080514-1542]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.ide' range='[3.4.1.M20080903-2000,3.4.1.M20080903-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.ide.application' range='[1.0.0.I20080603-2000,1.0.0.I20080603-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.win32' range='[3.2.100.v20080408-0800,3.2.100.v20080408-0800]'>
          <filter>
            (&amp;(osgi.ws=win32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.core' range='[3.2.201.R34x_v20080714,3.2.201.R34x_v20080714]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.scheduler' range='[3.2.100.v20080404,3.2.100.v20080404]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.ui' range='[3.2.100.v20080318,3.2.100.v20080318]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.workbench.compatibility' range='[3.2.0.I20080509-2000,3.2.0.I20080509-2000]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.hpux.ia64_32' range='[1.0.0.v20080604-1400,1.0.0.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=hpux)(osgi.arch=ia64_32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.hpux.PA_RISC' range='[1.0.0.v20080604-1400,1.0.0.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=hpux)(osgi.arch=PA_RISC))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.linux.ppc' range='[1.0.100.v20080604-1400,1.0.100.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.arch=ppc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.linux.x86' range='[1.2.0.v20080604-1400,1.2.0.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.linux.x86_64' range='[1.0.100.v20080604-1400,1.0.100.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=linux)(osgi.arch=x86_64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.solaris.sparc' range='[1.0.100.v20080604-1400,1.0.100.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=solaris)(osgi.arch=sparc))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.macosx' range='[1.0.0.v20080604-1400,1.0.0.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=macosx))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.qnx.x86' range='[1.0.0.v20080604-1400,1.0.0.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=qnx)(osgi.ws=photon)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.win32.x86' range='[1.1.0.v20080604-1400,1.1.0.v20080604-1400]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filesystem.win32.ia64' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=ia64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.resources.win32.x86' range='[3.4.0.v20071204,3.4.0.v20071204]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.resources.win32.ia64' range='0.0.0'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=ia64))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.core.linux' range='[3.2.0.v20080422,3.2.0.v20080422]'>
          <filter>
            (&amp;(osgi.os=linux))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.core.win32' range='[3.2.100.v20080107,3.2.100.v20080107]'>
          <filter>
            (&amp;(osgi.os=win32))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.views.properties.tabbed' range='[3.4.1.M20080730-0800,3.4.1.M20080730-0800]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security' range='[1.0.1.R34x_v20080721,1.0.1.R34x_v20080721]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security.ui' range='[1.0.0.v20080603-1810,1.0.0.v20080603-1810]'/>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security.win32.x86' range='[1.0.0.v20080529-1600,1.0.0.v20080529-1600]'>
          <filter>
            (&amp;(osgi.os=win32)(osgi.arch=x86))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security.macosx' range='[1.0.0.v20080602-2000,1.0.0.v20080602-2000]'>
          <filter>
            (&amp;(osgi.os=macosx))
          </filter>
        </required>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.feature.jar' range='[3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu,3.4.1.r341_v20080731-9I96EiDElYevwz-p1bP5z-NlAaP7vtX6Utotqsu]'>
          <filter>
            (org.eclipse.update.install.features=true)
          </filter>
        </required>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='org.apache.lucene' version='1.9.1.v20080530-1600' singleton='false'>
      <update id='org.apache.lucene' range='[0.0.0,1.9.1.v20080530-1600)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Apache Lucene'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='15'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.apache.lucene' version='1.9.1.v20080530-1600'/>
        <provided namespace='osgi.bundle' name='org.apache.lucene' version='1.9.1.v20080530-1600'/>
        <provided namespace='java.package' name='org.apache.lucene' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.de' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.standard' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.document' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.index' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.queryParser' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.search' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.search.spans' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.store' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.util' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.apache.lucene' version='1.9.1.v20080530-1600'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.9.1.v20080530-1600&#xA;Export-Package: org.apache.lucene,org.apache.lucene.analysis,org.apache.lucene.analysis.de,org.apache.lucene.analysis.standard,org.apache.lucene.document,org.apache.lucene.index,org.apache.lucene.queryParser,org.apache.lucene.search,org.apache.lucene.search.spans,org.apache.lucene.store,org.apache.lucene.util&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.apache.lucene&#xA;Bundle-Name: %pluginName&#xA;Bundle-Localization: plugin&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.swt.gtk.linux.x86' version='3.4.1.v3449c'>
      <update id='org.eclipse.swt.gtk.linux.x86' range='[0.0.0,3.4.1.v3449c)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.fragmentName' value='Standard Widget Toolkit for GTK 2.0'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%fragmentName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='12'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt.gtk.linux.x86' version='3.4.1.v3449c'/>
        <provided namespace='osgi.bundle' name='org.eclipse.swt.gtk.linux.x86' version='3.4.1.v3449c'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.accessibility.gtk' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.cairo' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.cde' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.gnome' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.gtk' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.mozilla' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.opengl.glx' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.swt' version='3.4.1.v3449c'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.swt' range='[3.0.0,4.0.0)'/>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.swt.gtk.linux.x86' version='3.4.1.v3449c'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: fragment&#xA;Bundle-Name: %fragmentName&#xA;Manifest-Version: 1.0&#xA;Fragment-Host: org.eclipse.swt; bundle-version=&quot;[3.0.0,4.0.0)&quot;&#xA;Bundle-SymbolicName: org.eclipse.swt.gtk.linux.x86; singleton:=true&#xA;Bundle-Version: 3.4.1.v3449c&#xA;Export-Package: org.eclipse.swt.internal.accessibility.gtk; x-internal:=true,org.eclipse.swt.internal.cairo; x-internal:=true,org.eclipse.swt.internal.cde; x-internal:=true,org.eclipse.swt.internal.gnome; x-internal:=true,org.eclipse.swt.internal.gtk; x-internal:=true,org.eclipse.swt.internal.mozilla; x-internal:=true,org.eclipse.swt.internal.opengl.glx; x-internal:=true&#xA;Eclipse-PlatformFilter: (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.mortbay.jetty' version='5.1.14.v200806031611' singleton='false'>
      <update id='org.mortbay.jetty' range='[0.0.0,5.1.14.v200806031611)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Jetty WebServer'/>
        <property name='df_LT.bundleProvider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%bundleProvider'/>
      </properties>
      <provides size='16'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.mortbay.jetty' version='5.1.14.v200806031611'/>
        <provided namespace='osgi.bundle' name='org.mortbay.jetty' version='5.1.14.v200806031611'/>
        <provided namespace='java.package' name='org.mortbay.html' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.http' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.http.ajp' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.http.handler' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.http.nio' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.jetty' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.jetty.servlet' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.jetty.win32' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.log' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.servlet' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.util' version='5.1.14'/>
        <provided namespace='java.package' name='org.mortbay.xml' version='5.1.14'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='java.package' name='javax.net.ssl' range='0.0.0'/>
        <required namespace='java.package' name='javax.security.cert' range='0.0.0'/>
        <required namespace='java.package' name='javax.servlet' range='[2.4.0,2.6.0)'/>
        <required namespace='java.package' name='javax.servlet.http' range='[2.4.0,2.6.0)'/>
        <required namespace='java.package' name='javax.servlet.resources' range='[2.4.0,2.6.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.apache.commons.logging' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.mortbay.jetty' version='5.1.14.v200806031611'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 5.1.14.v200806031611&#xA;Export-Package: org.mortbay.html;version=&quot;5.1.14&quot;,org.mortbay.http;version=&quot;5.1.14&quot;,org.mortbay.http.ajp;version=&quot;5.1.14&quot;,org.mortbay.http.handler;version=&quot;5.1.14&quot;,org.mortbay.http.nio;version=&quot;5.1.14&quot;,org.mortbay.jetty;version=&quot;5.1.14&quot;,org.mortbay.jetty.servlet;version=&quot;5.1.14&quot;,org.mortbay.jetty.win32;version=&quot;5.1.14&quot;,org.mortbay.log;version=&quot;5.1.14&quot;,org.mortbay.servlet;version=&quot;5.1.14&quot;,org.mortbay.util;version=&quot;5.1.14&quot;,org.mortbay.xml;version=&quot;5.1.14&quot;&#xA;Bundle-SymbolicName: org.mortbay.jetty&#xA;Import-Package: javax.net.ssl,javax.security.cert,javax.servlet;version=&quot;[2.4.0,2.6.0)&quot;,javax.servlet.http;version=&quot;[2.4.0,2.6.0)&quot;,javax.servlet.resources;version=&quot;[2.4.0,2.6.0)&quot;,javax.xml.parsers,org.apache.commons.logging;version=&quot;[1.0.0,2.0.0)&quot;,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %bundleName&#xA;Bundle-Vendor: %bundleProvider
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='com.ibm.icu' version='3.8.1.v20080530'>
      <update id='com.ibm.icu' range='[0.0.0,3.8.1.v20080530)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='IBM Corporation'/>
        <property name='df_LT.pluginName' value='International Components for Unicode for Java (ICU4J)'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='15'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='com.ibm.icu' version='3.8.1.v20080530'/>
        <provided namespace='osgi.bundle' name='com.ibm.icu' version='3.8.1.v20080530'/>
        <provided namespace='java.package' name='com.ibm.icu.lang' version='3.8.1'/>
        <provided namespace='java.package' name='com.ibm.icu.math' version='3.8.1'/>
        <provided namespace='java.package' name='com.ibm.icu.text' version='3.8.1'/>
        <provided namespace='java.package' name='com.ibm.icu.util' version='3.8.1'/>
        <provided namespace='java.package' name='com.ibm.icu.impl' version='0.0.0'/>
        <provided namespace='java.package' name='com.ibm.icu.impl.data' version='0.0.0'/>
        <provided namespace='java.package' name='com.ibm.icu.impl.data.icudt38b' version='0.0.0'/>
        <provided namespace='java.package' name='com.ibm.icu.impl.data.icudt38b.brkitr' version='0.0.0'/>
        <provided namespace='java.package' name='com.ibm.icu.impl.data.icudt38b.coll' version='0.0.0'/>
        <provided namespace='java.package' name='com.ibm.icu.impl.data.icudt38b.rbnf' version='0.0.0'/>
        <provided namespace='java.package' name='com.ibm.icu.impl.data.icudt38b.translit' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='com.ibm.icu' version='3.8.1.v20080530'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ClassPath: icu-jse4.jar,icu-data.jar,.&#xA;Bundle-SymbolicName: com.ibm.icu; singleton:=true&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.8.1.v20080530&#xA;Bundle-Localization: plugin&#xA;Bundle-Copyright: Licensed Materials - Property of IBM (C) Copyright IBM Corp. 2000, 2008. All Rights Reserved. IBM is a registered trademark of IBM Corp.&#xA;Eclipse-ExtensibleAPI: true&#xA;Export-Package: com.ibm.icu.lang;version=&quot;3.8.1&quot;,com.ibm.icu.math;version=&quot;3.8.1&quot;,com.ibm.icu.text;version=&quot;3.8.1&quot;,com.ibm.icu.util;version=&quot;3.8.1&quot;,com.ibm.icu.impl;x-internal:=true,com.ibm.icu.impl.data;x-internal:=true,com.ibm.icu.impl.data.icudt38b;x-internal:=true,com.ibm.icu.impl.data.icudt38b.brkitr;x-internal:=true,com.ibm.icu.impl.data.icudt38b.coll;x-internal:=true,com.ibm.icu.impl.data.icudt38b.rbnf;x-internal:=true,com.ibm.icu.impl.data.icudt38b.translit;x-internal:=true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.filebuffers' version='3.4.0.v20080603-2000'>
      <update id='org.eclipse.core.filebuffers' range='[0.0.0,3.4.0.v20080603-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='File Buffers'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.filebuffers' version='3.4.0.v20080603-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.filebuffers' version='3.4.0.v20080603-2000'/>
        <provided namespace='java.package' name='org.eclipse.core.filebuffers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.filebuffers.manipulation' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.filebuffers' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.text' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.filebuffers' version='3.4.0.v20080603-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.core.internal.filebuffers.FileBuffersPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.core.filebuffers; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.0.v20080603-2000&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.filebuffers,org.eclipse.core.filebuffers.manipulation,org.eclipse.core.internal.filebuffers;x-internal:=true&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.net' version='1.0.0.I20080605'>
      <update id='org.eclipse.ui.net' range='[0.0.0,1.0.0.I20080605)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.PLUGIN_NAME' value='Internet Connection Management UI'/>
        <property name='df_LT.PLUGIN_PROVIDER' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%PLUGIN_NAME'/>
        <property name='org.eclipse.equinox.p2.provider' value='%PLUGIN_PROVIDER'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.net' version='1.0.0.I20080605'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.net' version='1.0.0.I20080605'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.net' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.net.auth' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.net' range='[1.0.0,2.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.net' version='1.0.0.I20080605'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.ui.internal.net;x-internal:=true,org.eclipse.ui.internal.net.auth;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.net;bundle-version=&quot;[1.0.0,2.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.ui.internal.net.Activator&#xA;Bundle-Name: %PLUGIN_NAME&#xA;Bundle-Version: 1.0.0.I20080605&#xA;Bundle-Vendor: %PLUGIN_PROVIDER&#xA;Bundle-SymbolicName: org.eclipse.ui.net; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.exemplarysetup' version='1.0.0.v20080427-2136'>
      <update id='org.eclipse.equinox.p2.exemplarysetup' range='[0.0.0,1.0.0.v20080427-2136)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Exemplary Setup'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.exemplarysetup' version='1.0.0.v20080427-2136'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.exemplarysetup' version='1.0.0.v20080427-2136'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.exemplarysetup' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='13'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.garbagecollector' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.exemplarysetup' version='1.0.0.v20080427-2136'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.exemplarysetup.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.exemplarysetup;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core,org.eclipse.equinox.internal.p2.director,org.eclipse.equinox.internal.p2.engine,org.eclipse.equinox.internal.p2.garbagecollector,org.eclipse.equinox.internal.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.osgi.framework;version=&quot;1.3.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.0.v20080427-2136&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.exemplarysetup;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.team.ui' version='3.4.1.r34x_20080827'>
      <update id='org.eclipse.team.ui' range='[0.0.0,3.4.1.r34x_20080827)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Team Support UI'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='18'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.team.ui' version='3.4.1.r34x_20080827'/>
        <provided namespace='osgi.bundle' name='org.eclipse.team.ui' version='3.4.1.r34x_20080827'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.mapping' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.registry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.synchronize' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.synchronize.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.ui.wizards' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.ui.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.ui.mapping' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.ui.synchronize' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='13'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.3.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.team.core' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.compare' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.navigator' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.navigator.resources' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.editors' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='com.ibm.icu.util' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.team.ui' version='3.4.1.r34x_20080827'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.team.internal.ui.TeamUIPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.team.ui; singleton:=true&#xA;Import-Package: com.ibm.icu.text,com.ibm.icu.util&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.4.1.r34x_20080827&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.team.internal.ui;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.actions;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.dialogs;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.history;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.mapping;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.preferences;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.registry;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.synchronize;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.synchronize.actions;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.internal.ui.wizards;x-friends:=&quot;org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui&quot;,org.eclipse.team.ui,org.eclipse.team.ui.history,org.eclipse.team.ui.mapping,org.eclipse.team.ui.synchronize&#xA;Require-Bundle: org.eclipse.ui.ide;bundle-version=&quot;[3.3.0,4.0.0)&quot;;resolution:=optional,org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.team.core;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.compare;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.navigator;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.navigator.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.editors;bundle-version=&quot;[3.3.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.navigator.resources' version='3.3.101.M20080910-0800'>
      <update id='org.eclipse.ui.navigator.resources' range='[0.0.0,3.3.101.M20080910-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Navigator Workbench Components'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='9'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.navigator.resources' version='3.3.101.M20080910-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.navigator.resources' version='3.3.101.M20080910-0800'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.resources.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.resources.plugin' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.resources.workbench' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.workingsets' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.navigator.resources' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.navigator' range='[3.2.1,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.views.properties.tabbed' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.navigator.resources' version='3.3.101.M20080910-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.ui.internal.navigator.resources.actions;x-internal:=true,org.eclipse.ui.internal.navigator.resources.plugin;x-internal:=true,org.eclipse.ui.internal.navigator.resources.workbench;x-internal:=true,org.eclipse.ui.internal.navigator.workingsets;x-internal:=true,org.eclipse.ui.navigator.resources&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.ui.ide;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.jface;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.navigator;bundle-version=&quot;[3.2.1,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.views.properties.tabbed;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.workbench.texteditor;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.ui.internal.navigator.resources.plugin.WorkbenchNavigatorPlugin&#xA;Bundle-Name: %Plugin.name&#xA;Bundle-Version: 3.3.101.M20080910-0800&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Bundle-SymbolicName: org.eclipse.ui.navigator.resources; singleton:=true&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.search' version='3.4.1.r341_v20080813-0800'>
      <update id='org.eclipse.search' range='[0.0.0,3.4.1.r341_v20080813-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Search Support'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='16'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.search' version='3.4.1.r341_v20080813-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.search' version='3.4.1.r341_v20080813-0800'/>
        <provided namespace='java.package' name='org.eclipse.search.core.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.internal.core.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.internal.ui.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.internal.ui.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search.ui.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search2.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search2.internal.ui.basic.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search2.internal.ui.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.search2.internal.ui.text2' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filebuffers' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ltk.core.refactoring' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ltk.ui.refactoring' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.search' version='3.4.1.r341_v20080813-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.search.internal.ui.SearchPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.search; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.r341_v20080813-0800&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.search.core.text,org.eclipse.search.internal.core.text;x-internal:=true,org.eclipse.search.internal.ui;x-internal:=true,org.eclipse.search.internal.ui.text;x-internal:=true,org.eclipse.search.internal.ui.util;x-internal:=true,org.eclipse.search.ui,org.eclipse.search.ui.actions,org.eclipse.search.ui.text,org.eclipse.search2.internal.ui;x-internal:=true,org.eclipse.search2.internal.ui.basic.views;x-internal:=true,org.eclipse.search2.internal.ui.text;x-internal:=true,org.eclipse.search2.internal.ui.text2;x-internal:=true&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.filebuffers;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.ide;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.workbench.texteditor;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ltk.core.refactoring;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ltk.ui.refactoring;bundle-version=&quot;[3.4.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.directorywatcher' version='1.0.2.v20080806-1619'>
      <update id='org.eclipse.equinox.p2.directorywatcher' range='[0.0.0,1.0.2.v20080806-1619)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Directory Watcher'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.directorywatcher' version='1.0.2.v20080806-1619'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.directorywatcher' version='1.0.2.v20080806-1619'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.directorywatcher' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='18'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.generator.features' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.generator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.4.0'/>
        <required namespace='java.package' name='org.osgi.service.cm' range='1.2.0' optional='true'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.directorywatcher' version='1.0.2.v20080806-1619'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.provisional.p2.directorywatcher.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.directorywatcher;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.metadata.generator.features,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.generator,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util,org.osgi.framework;version=&quot;1.4.0&quot;,org.osgi.service.cm;version=&quot;1.2.0&quot;;resolution:=optional,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.util.tracker;version=&quot;1.3.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.0.2.v20080806-1619&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.provisional.p2.directorywatcher;x-friends:=&quot;org.eclipse.equinox.p2.reconciler.dropins,org.eclipse.equinox.p2.extensionlocation&quot;&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.3.0,4.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.reconciler.dropins' version='1.0.2.R34x_v20080909'>
      <update id='org.eclipse.equinox.p2.reconciler.dropins' range='[0.0.0,1.0.2.R34x_v20080909)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Dropin Reconciler Plug-in'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.reconciler.dropins' version='1.0.2.R34x_v20080909'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.reconciler.dropins' version='1.0.2.R34x_v20080909'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.reconciler.dropins' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='29'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.touchpoint.eclipse' range='0.1.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.app' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.extensionlocation' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.update' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.configurator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.directorywatcher' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.3'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.reconciler.dropins' version='1.0.2.R34x_v20080909'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.p2.reconciler.dropins;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.p2.touchpoint.eclipse;bundle-version=&quot;0.1.0&quot;&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.reconciler.dropins.Activator&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.2.R34x_v20080909&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.reconciler.dropins;singleton:=true&#xA;Import-Package: org.eclipse.equinox.app;version=&quot;1.0.0&quot;,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.extensionlocation,org.eclipse.equinox.internal.p2.artifact.repository,org.eclipse.equinox.internal.p2.metadata.repository,org.eclipse.equinox.internal.p2.update,org.eclipse.equinox.internal.provisional.configurator,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.directorywatcher,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.environment;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;,org.xml.sax&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.runtime' version='3.4.0.v20080512'>
      <update id='org.eclipse.core.runtime' range='[0.0.0,3.4.0.v20080512)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Core Runtime'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime' version='3.4.0.v20080512'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.runtime' version='3.4.0.v20080512'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.preferences.legacy' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.runtime' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime' version='3.4.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.jobs' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.preferences' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.contenttype' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime.compatibility.auth' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.app' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.core.internal.runtime.auth' range='0.0.0' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.runtime' version='3.4.0.v20080512'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.core.internal.runtime.PlatformActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.core.runtime; singleton:=true&#xA;Bundle-ManifestVersion: 2&#xA;DynamicImport-Package: org.eclipse.core.internal.runtime.auth&#xA;Bundle-Version: 3.4.0.v20080512&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.internal.preferences.legacy;x-internal:=true,org.eclipse.core.internal.runtime;x-friends:=&quot;org.eclipse.core.runtime.compatibility&quot;,org.eclipse.core.runtime;version=&quot;3.4.0&quot;&#xA;Require-Bundle: org.eclipse.osgi;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.jobs;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.equinox.registry;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.equinox.preferences;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.contenttype;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.runtime.compatibility.auth;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.equinox.app;bundle-version=&quot;[1.0.0,2.0.0)&quot;;visibility:=reexport
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolinggtk.linux.x86org.eclipse.equinox.common' version='3.4.0.v20080421-2006' singleton='false'>
      <hostRequirements size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='3.4.0.v20080421-2006'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86org.eclipse.equinox.common' version='3.4.0.v20080421-2006'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='toolinggtk.linux.x86' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='3.4.0.v20080421-2006'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            setStartLevel(startLevel:2);markStarted(started: true);
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>
            setStartLevel(startLevel:-1);markStarted(started: false);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.debug.core' version='3.4.0.v20080612'>
      <update id='org.eclipse.debug.core' range='[0.0.0,3.4.0.v20080612)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Debug Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='13'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.debug.core' version='3.4.0.v20080612'/>
        <provided namespace='osgi.bundle' name='org.eclipse.debug.core' version='3.4.0.v20080612'/>
        <provided namespace='java.package' name='org.eclipse.debug.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.core.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.core.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.core.sourcelookup' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.core.sourcelookup.containers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.core.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.core.sourcelookup' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.core.sourcelookup.containers' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='6'>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.variables' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.debug.core' version='3.4.0.v20080612'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 3.4.0.v20080612&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-Activator: org.eclipse.debug.core.DebugPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.variables;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.4.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.debug.core,org.eclipse.debug.core.commands,org.eclipse.debug.core.model,org.eclipse.debug.core.sourcelookup,org.eclipse.debug.core.sourcelookup.containers,org.eclipse.debug.internal.core;x-friends:=&quot;org.eclipse.debug.ui&quot;,org.eclipse.debug.internal.core.commands;x-friends:=&quot;org.eclipse.debug.ui&quot;,org.eclipse.debug.internal.core.sourcelookup;x-friends:=&quot;org.eclipse.debug.ui&quot;,org.eclipse.debug.internal.core.sourcelookup.containers;x-friends:=&quot;org.eclipse.debug.ui&quot;&#xA;Bundle-SymbolicName: org.eclipse.debug.core; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.touchpoint.eclipse' version='1.0.2.R34x_v20080910'>
      <update id='org.eclipse.equinox.p2.touchpoint.eclipse' range='[0.0.0,1.0.2.R34x_v20080910)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Eclipse Touchpoint'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.touchpoint.eclipse' version='1.0.2.R34x_v20080910'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.touchpoint.eclipse' version='1.0.2.R34x_v20080910'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.touchpoint.eclipse' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.touchpoint.eclipse.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.update' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='25'>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.garbagecollector' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.frameworkadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.generator' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.simpleconfigurator.manipulator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.2.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.2'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.touchpoint.eclipse' version='1.0.2.R34x_v20080910'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.p2.touchpoint.eclipse;x-friends:=&quot;org.eclipse.equinox.p2.reconciler.dropins,org.eclipse.equinox.p2.extensionlocation&quot;,org.eclipse.equinox.internal.p2.touchpoint.eclipse.actions;x-internal:=true,org.eclipse.equinox.internal.p2.update;x-friends:=&quot;org.eclipse.equinox.p2.reconciler.dropins,org.eclipse.equinox.p2.extensionlocation&quot;&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Localization: plugin&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.touchpoint.eclipse.Activator&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.2.R34x_v20080910&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.touchpoint.eclipse;singleton:=true&#xA;Import-Package: javax.xml.parsers,org.eclipse.core.runtime;common=split,org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.engine,org.eclipse.equinox.internal.p2.garbagecollector,org.eclipse.equinox.internal.provisional.frameworkadmin,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.generator;resolution:=optional,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository,org.eclipse.equinox.internal.simpleconfigurator.manipulator,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.environment;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.2.0&quot;;resolution:=optional,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.2&quot;,org.w3c.dom,org.xml.sax&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.http.jetty' version='1.1.0.v20080425' singleton='false'>
      <update id='org.eclipse.equinox.http.jetty' range='[0.0.0,1.1.0.v20080425)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Jetty Http Service'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.http.jetty' version='1.1.0.v20080425'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.http.jetty' version='1.1.0.v20080425'/>
        <provided namespace='java.package' name='org.eclipse.equinox.http.jetty' version='1.1.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='java.package' name='javax.servlet' range='[2.4.0,2.6.0)'/>
        <required namespace='java.package' name='javax.servlet.http' range='[2.4.0,2.6.0)'/>
        <required namespace='java.package' name='org.eclipse.equinox.http.servlet' range='1.0.0'/>
        <required namespace='java.package' name='org.mortbay.http' range='[5.1.0,6.0.0)'/>
        <required namespace='java.package' name='org.mortbay.jetty.servlet' range='[5.1.0,6.0.0)'/>
        <required namespace='java.package' name='org.mortbay.util' range='[5.1.0,6.0.0)'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.cm' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.service.startlevel' range='1.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.http.jetty' version='1.1.0.v20080425'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %bundleName&#xA;Bundle-Version: 1.1.0.v20080425&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.servlet;version=&quot;[2.4.0,2.6.0)&quot;,javax.servlet.http;version=&quot;[2.4.0,2.6.0)&quot;,org.eclipse.equinox.http.servlet;version=&quot;1.0.0&quot;,org.mortbay.http;version=&quot;[5.1.0,6.0.0)&quot;,org.mortbay.jetty.servlet;version=&quot;[5.1.0,6.0.0)&quot;,org.mortbay.util;version=&quot;[5.1.0,6.0.0)&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.cm;version=&quot;1.2.0&quot;,org.osgi.service.startlevel;version=&quot;1.0&quot;&#xA;Bundle-Activator: org.eclipse.equinox.http.jetty.internal.Activator&#xA;Bundle-Vendor: %providerName&#xA;Export-Package: org.eclipse.equinox.http.jetty;version=&quot;1.1.0&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.http.jetty&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.intro.universal' version='3.2.200.v20080508'>
      <update id='org.eclipse.ui.intro.universal' range='[0.0.0,3.2.200.v20080508)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin_name' value='Universal Welcome'/>
        <property name='df_LT.provider_name' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%provider_name'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.intro.universal' version='3.2.200.v20080508'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.intro.universal' version='3.2.200.v20080508'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.universal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.universal.contentdetect' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro.universal.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.intro.universal' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.intro' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.intro.universal' version='3.2.200.v20080508'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-ActivationPolicy: lazy;exclude:=&quot;org.eclipse.ui.internal.intro.universal.contentdetect&quot;&#xA;Bundle-Name: %plugin_name&#xA;Bundle-ClassPath: universal.jar&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.intro;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.ui.internal.intro.universal.UniversalIntroPlugin&#xA;Bundle-Vendor: %provider_name&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 3.2.200.v20080508&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.internal.intro.universal;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.intro.universal.contentdetect;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.ui.internal.intro.universal.util;x-internal:=true,org.eclipse.ui.intro.universal&#xA;Bundle-SymbolicName: org.eclipse.ui.intro.universal;singleton:=true&#xA;Import-Package: javax.xml.parsers,org.w3c.dom,org.xml.sax&#xA;Eclipse-LazyStart: true; exceptions=&quot;org.eclipse.ui.internal.intro.universal.contentdetect&quot;
          </instruction>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.debug.ui' version='3.4.1.v20080811_r341'>
      <update id='org.eclipse.debug.ui' range='[0.0.0,3.4.1.v20080811_r341)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Debug UI'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='47'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.debug.ui' version='3.4.1.v20080811_r341'/>
        <provided namespace='osgi.bundle' name='org.eclipse.debug.ui' version='3.4.1.v20080811_r341'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.actions.breakpointGroups' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.actions.breakpoints' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.actions.expressions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.actions.variables' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.actions.variables.details' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.commands.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.contextlaunching' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.contexts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.elements.adapters' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.importexport.breakpoints' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.importexport.launchconfigurations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.launchConfigurations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.memory' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.memory.provisional' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.model.elements' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.sourcelookup' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.sourcelookup.browsers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.stringsubstitution' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.viewers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.viewers.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.viewers.model.provisional' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.viewers.provisional' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.viewers.update' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.breakpoints' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.console' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.expression' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.launch' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.memory' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.memory.renderings' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.modules' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.registers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.variables' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.internal.ui.views.variables.details' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.ui.console' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.ui.contexts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.ui.memory' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.debug.ui.sourcelookup' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='13'>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.variables' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.console' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.debug.core' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.editors' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.debug.ui' version='3.4.1.v20080811_r341'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 3.4.1.v20080811_r341&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-Activator: org.eclipse.debug.internal.ui.DebugUIPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.core.expressions;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.variables;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui.console;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.debug.core;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.jface.text;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.workbench.texteditor;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.ide;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.editors;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;&#xA;Export-Package: org.eclipse.debug.internal.ui;x-internal:=true,org.eclipse.debug.internal.ui.actions;x-internal:=true,org.eclipse.debug.internal.ui.actions.breakpointGroups;x-internal:=true,org.eclipse.debug.internal.ui.actions.breakpoints;x-internal:=true,org.eclipse.debug.internal.ui.actions.expressions;x-internal:=true,org.eclipse.debug.internal.ui.actions.variables;x-internal:=true,org.eclipse.debug.internal.ui.actions.variables.details;x-internal:=true,org.eclipse.debug.internal.ui.commands.actions;x-internal:=true,org.eclipse.debug.internal.ui.contextlaunching;x-internal:=true,org.eclipse.debug.internal.ui.contexts;x-internal:=true,org.eclipse.debug.internal.ui.elements.adapters;x-internal:=true,org.eclipse.debug.internal.ui.importexport.breakpoints;x-internal:=true,org.eclipse.debug.internal.ui.importexport.launchconfigurations;x-internal:=true,org.eclipse.debug.internal.ui.launchConfigurations;x-internal:=true,org.eclipse.debug.internal.ui.memory;x-internal:=true,org.eclipse.debug.internal.ui.memory.provisional;x-internal:=true,org.eclipse.debug.internal.ui.model.elements;x-internal:=true,org.eclipse.debug.internal.ui.preferences;x-internal:=true,org.eclipse.debug.internal.ui.sourcelookup;x-internal:=true,org.eclipse.debug.internal.ui.sourcelookup.browsers;x-internal:=true,org.eclipse.debug.internal.ui.stringsubstitution;x-internal:=true,org.eclipse.debug.internal.ui.viewers;x-internal:=true,org.eclipse.debug.internal.ui.viewers.model;x-internal:=true,org.eclipse.debug.internal.ui.viewers.model.provisional;x-internal:=true,org.eclipse.debug.internal.ui.viewers.provisional;x-internal:=true,org.eclipse.debug.internal.ui.viewers.update;x-internal:=true,org.eclipse.debug.internal.ui.views;x-internal:=true,org.eclipse.debug.internal.ui.views.breakpoints;x-internal:=true,org.eclipse.debug.internal.ui.views.console;x-internal:=true,org.eclipse.debug.internal.ui.views.expression;x-internal:=true,org.eclipse.debug.internal.ui.views.launch;x-internal:=true,org.eclipse.debug.internal.ui.views.memory;x-internal:=true,org.eclipse.debug.internal.ui.views.memory.renderings;x-internal:=true,org.eclipse.debug.internal.ui.views.modules;x-internal:=true,org.eclipse.debug.internal.ui.views.registers;x-internal:=true,org.eclipse.debug.internal.ui.views.variables;x-internal:=true,org.eclipse.debug.internal.ui.views.variables.details;x-internal:=true,org.eclipse.debug.ui,org.eclipse.debug.ui.actions,org.eclipse.debug.ui.console,org.eclipse.debug.ui.contexts,org.eclipse.debug.ui.memory,org.eclipse.debug.ui.sourcelookup&#xA;Bundle-SymbolicName: org.eclipse.debug.ui; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.resources.compatibility' version='3.4.0.v20080604-1400' singleton='false'>
      <update id='org.eclipse.core.resources.compatibility' range='[0.0.0,3.4.0.v20080604-1400)' severity='0'/>
      <properties size='2'>
        <property name='org.eclipse.equinox.p2.name' value='%compatibilityFragmentName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.resources.compatibility' version='3.4.0.v20080604-1400'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.resources.compatibility' version='3.4.0.v20080604-1400'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.indexing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.localstore' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.properties' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.resources' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.core.resources' version='3.4.0.v20080604-1400'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.4.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.resources.compatibility' version='3.4.0.v20080604-1400'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Fragment-Host: org.eclipse.core.resources;bundle-version=&quot;[3.4.0,4.0.0)&quot;&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 3.4.0.v20080604-1400&#xA;Export-Package: org.eclipse.core.internal.indexing;x-internal:=true,org.eclipse.core.internal.localstore;x-internal:=true,org.eclipse.core.internal.properties;x-internal:=true,org.eclipse.core.internal.resources;x-internal:=true&#xA;Bundle-SymbolicName: org.eclipse.core.resources.compatibility&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %compatibilityFragmentName&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.apache.commons.el' version='1.0.0.v200806031608' singleton='false'>
      <update id='org.apache.commons.el' range='[0.0.0,1.0.0.v200806031608)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Apache Commons JSP 2.0 Expression Language Interpreter'/>
        <property name='df_LT.bundleProvider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%bundleProvider'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.apache.commons.el' version='1.0.0.v200806031608'/>
        <provided namespace='osgi.bundle' name='org.apache.commons.el' version='1.0.0.v200806031608'/>
        <provided namespace='java.package' name='org.apache.commons.el' version='1.0.0'/>
        <provided namespace='java.package' name='org.apache.commons.el.parser' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='java.package' name='javax.servlet' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.jsp' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.jsp.el' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.jsp.resources' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.jsp.tagext' range='2.0.0'/>
        <required namespace='java.package' name='javax.servlet.resources' range='2.4.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.apache.commons.el' version='1.0.0.v200806031608'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 1.0.0.v200806031608&#xA;Export-Package: org.apache.commons.el;version=&quot;1.0.0&quot;,org.apache.commons.el.parser;version=&quot;1.0.0&quot;&#xA;Bundle-SymbolicName: org.apache.commons.el&#xA;Import-Package: javax.servlet;version=&quot;2.4&quot;,javax.servlet.http;version=&quot;2.4&quot;,javax.servlet.jsp;version=&quot;2.0&quot;,javax.servlet.jsp.el;version=&quot;2.0&quot;,javax.servlet.jsp.resources;version=&quot;2.0&quot;,javax.servlet.jsp.tagext;version=&quot;2.0&quot;,javax.servlet.resources;version=&quot;2.4&quot;&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %bundleName&#xA;Bundle-Vendor: %bundleProvider
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolinggtk.linux.x86org.eclipse.core.runtime' version='3.4.0.v20080512' singleton='false'>
      <hostRequirements size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='3.4.0.v20080512'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86org.eclipse.core.runtime' version='3.4.0.v20080512'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='toolinggtk.linux.x86' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='3.4.0.v20080512'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            markStarted(started: true);
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>
            markStarted(started: false);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='javax.servlet.jsp' version='2.0.0.v200806031607' singleton='false'>
      <update id='javax.servlet.jsp' range='[0.0.0,2.0.0.v200806031607)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Java Server Pages API Bundle'/>
        <property name='df_LT.bundleProvider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%bundleProvider'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='javax.servlet.jsp' version='2.0.0.v200806031607'/>
        <provided namespace='osgi.bundle' name='javax.servlet.jsp' version='2.0.0.v200806031607'/>
        <provided namespace='java.package' name='javax.servlet.jsp' version='2.0.0'/>
        <provided namespace='java.package' name='javax.servlet.jsp.el' version='2.0.0'/>
        <provided namespace='java.package' name='javax.servlet.jsp.resources' version='2.0.0'/>
        <provided namespace='java.package' name='javax.servlet.jsp.tagext' version='2.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='java.package' name='javax.servlet' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.resources' range='2.4.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='javax.servlet.jsp' version='2.0.0.v200806031607'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 2.0.0.v200806031607&#xA;Export-Package: javax.servlet.jsp; version=2.0,javax.servlet.jsp.el; version=2.0,javax.servlet.jsp.resources; version=2.0,javax.servlet.jsp.tagext; version=2.0&#xA;Bundle-SymbolicName: javax.servlet.jsp&#xA;Import-Package: javax.servlet; version=2.4,javax.servlet.http; version=2.4,javax.servlet.resources; version=2.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %bundleName&#xA;Bundle-Vendor: %bundleProvider
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui' version='3.4.1.M20080910-0800'>
      <update id='org.eclipse.ui' range='[0.0.0,3.4.1.M20080910-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Eclipse UI'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui' version='3.4.1.M20080910-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui' version='3.4.1.M20080910-0800'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.swt' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.4.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui' version='3.4.1.M20080910-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %Plugin.name&#xA;Bundle-Activator: org.eclipse.ui.internal.UIPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Bundle-ClassPath: .&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui; singleton:=true&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.M20080910-0800&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.internal;x-internal:=true&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.swt;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.jface;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.ui.workbench;bundle-version=&quot;[3.4.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.core.expressions;bundle-version=&quot;[3.4.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.osgi' version='3.4.2.R34x_v20080826-1230'>
      <update id='org.eclipse.osgi' range='[0.0.0,3.4.2.R34x_v20080826-1230)' severity='0'/>
      <properties size='5'>
        <property name='df_LT.systemBundle' value='OSGi System Bundle'/>
        <property name='df_LT.eclipse.org' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%systemBundle'/>
        <property name='org.eclipse.equinox.p2.description' value='%systemBundle'/>
        <property name='org.eclipse.equinox.p2.provider' value='%eclipse.org'/>
      </properties>
      <provides size='52'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.osgi' version='3.4.2.R34x_v20080826-1230'/>
        <provided namespace='osgi.bundle' name='org.eclipse.osgi' version='3.4.2.R34x_v20080826-1230'/>
        <provided namespace='java.package' name='org.eclipse.osgi.event' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.console' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.eventmgr' version='1.1.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.log' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.datalocation' version='1.1.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.debug' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.environment' version='1.1.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.localization' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.pluginconversion' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.resolver' version='1.2.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.runnable' version='1.1.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.security' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.service.urlconversion' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.signedcontent' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.storagemanager' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.util' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.framework' version='1.4.0'/>
        <provided namespace='java.package' name='org.osgi.service.condpermadmin' version='1.0.0'/>
        <provided namespace='java.package' name='org.osgi.service.packageadmin' version='1.2.0'/>
        <provided namespace='java.package' name='org.osgi.service.permissionadmin' version='1.2.0'/>
        <provided namespace='java.package' name='org.osgi.service.startlevel' version='1.1.0'/>
        <provided namespace='java.package' name='org.osgi.service.url' version='1.0.0'/>
        <provided namespace='java.package' name='org.osgi.util.tracker' version='1.3.3'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.adaptor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.internal.adaptor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.internal.stats' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.baseadaptor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.baseadaptor.bundlefile' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.baseadaptor.hooks' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.baseadaptor.loader' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.adaptor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.debug' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.internal.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.internal.protocol' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.internal.protocol.bundleentry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.internal.protocol.bundleresource' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.internal.protocol.reference' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.internal.reliablefile' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.launcher' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.framework.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.baseadaptor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.module' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.profile' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.resolver' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.provisional.service.security' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.provisional.verifier' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.service.security' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.osgi.internal.signedcontent' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.osgi' version='3.4.2.R34x_v20080826-1230'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Version: 3.4.2.R34x_v20080826-1230&#xA;Bundle-DocUrl: http://www.eclipse.org&#xA;Bundle-Copyright: %copyright&#xA;Eclipse-ExtensibleAPI: true&#xA;Eclipse-SystemBundle: true&#xA;Export-Package: org.eclipse.osgi.event;version=&quot;1.0&quot;,org.eclipse.osgi.framework.console;version=&quot;1.0&quot;,org.eclipse.osgi.framework.eventmgr;version=&quot;1.1&quot;,org.eclipse.osgi.framework.log;version=&quot;1.0&quot;,org.eclipse.osgi.service.datalocation;version=&quot;1.1&quot;,org.eclipse.osgi.service.debug;version=&quot;1.0&quot;,org.eclipse.osgi.service.environment;version=&quot;1.1&quot;,org.eclipse.osgi.service.localization;version=&quot;1.0&quot;,org.eclipse.osgi.service.pluginconversion;version=&quot;1.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.2&quot;,org.eclipse.osgi.service.runnable;version=&quot;1.1&quot;,org.eclipse.osgi.service.security; version=&quot;1.0&quot;,org.eclipse.osgi.service.urlconversion;version=&quot;1.0&quot;,org.eclipse.osgi.signedcontent; version=&quot;1.0&quot;,org.eclipse.osgi.storagemanager;version=&quot;1.0&quot;,org.eclipse.osgi.util;version=&quot;1.1&quot;,org.osgi.framework;version=&quot;1.4&quot;,org.osgi.service.condpermadmin;version=&quot;1.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2&quot;,org.osgi.service.permissionadmin;version=&quot;1.2&quot;,org.osgi.service.startlevel;version=&quot;1.1&quot;,org.osgi.service.url;version=&quot;1.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;,org.eclipse.core.runtime.adaptor;x-friends:=&quot;org.eclipse.core.runtime&quot;,org.eclipse.core.runtime.internal.adaptor;x-internal:=true,org.eclipse.core.runtime.internal.stats;x-friends:=&quot;org.eclipse.core.runtime&quot;,org.eclipse.osgi.baseadaptor;x-internal:=true,org.eclipse.osgi.baseadaptor.bundlefile;x-internal:=true,org.eclipse.osgi.baseadaptor.hooks;x-internal:=true,org.eclipse.osgi.baseadaptor.loader;x-internal:=true,org.eclipse.osgi.framework.adaptor;x-internal:=true,org.eclipse.osgi.framework.debug;x-internal:=true,org.eclipse.osgi.framework.internal.core;x-internal:=true,org.eclipse.osgi.framework.internal.protocol;x-internal:=true,org.eclipse.osgi.framework.internal.protocol.bundleentry;x-internal:=true,org.eclipse.osgi.framework.internal.protocol.bundleresource;x-internal:=true,org.eclipse.osgi.framework.internal.protocol.reference;x-internal:=true,org.eclipse.osgi.framework.internal.reliablefile;x-internal:=true,org.eclipse.osgi.framework.launcher;x-internal:=true,org.eclipse.osgi.framework.util;x-internal:=true,org.eclipse.osgi.internal.baseadaptor;x-internal:=true,org.eclipse.osgi.internal.module;x-internal:=true,org.eclipse.osgi.internal.profile;x-internal:=true,org.eclipse.osgi.internal.resolver;x-internal:=true,org.eclipse.osgi.internal.provisional.service.security; x-friends:=&quot;org.eclipse.equinox.security.ui&quot;;version=&quot;1.0.0&quot;,org.eclipse.osgi.internal.provisional.verifier;x-friends:=&quot;org.eclipse.update.core,org.eclipse.ui.workbench,org.eclipse.equinox.p2.artifact.repository&quot;,org.eclipse.osgi.internal.service.security;x-friends:=&quot;org.eclipse.equinox.security.ui&quot;,org.eclipse.osgi.internal.signedcontent; x-internal:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-Activator: org.eclipse.osgi.framework.internal.core.SystemBundleActivator&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.5,OSGi/Minimum-1.1&#xA;Bundle-Localization: systembundle&#xA;Bundle-SymbolicName: org.eclipse.osgi; singleton:=true&#xA;Export-Service: org.osgi.service.packageadmin.PackageAdmin,org.osgi.service.permissionadmin.PermissionAdmin,org.osgi.service.startlevel.StartLevel,org.eclipse.osgi.service.debug.DebugOptions&#xA;Main-Class: org.eclipse.core.runtime.adaptor.EclipseStarter&#xA;Bundle-Description: %systemBundle&#xA;Bundle-Vendor: %eclipse.org&#xA;Bundle-Name: %systemBundle&#xA;Bundle-ManifestVersion: 2
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.variables' version='3.2.100.v20080529-1300'>
      <update id='org.eclipse.core.variables' range='[0.0.0,3.2.100.v20080529-1300)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Core Variables'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.variables' version='3.2.100.v20080529-1300'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.variables' version='3.2.100.v20080529-1300'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.variables' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.variables' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.variables' version='3.2.100.v20080529-1300'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.core.internal.variables;x-internal:=true,org.eclipse.core.variables&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.core.variables.VariablesPlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.2.100.v20080529-1300&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.variables; singleton:=true&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.runtime.compatibility' version='3.2.0.v20071008'>
      <update id='org.eclipse.core.runtime.compatibility' range='[0.0.0,3.2.0.v20071008)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Core Runtime Plug-in Compatibility'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='10'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime.compatibility' version='3.2.0.v20071008'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.runtime.compatibility' version='3.2.0.v20071008'/>
        <provided namespace='java.package' name='org.eclipse.core.boot' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.boot' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.compatibility' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.plugins' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.model' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='[3.1.100,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.runtime.compatibility' version='3.2.0.v20071008'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.core.internal.plugins.CompatibilityActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.runtime.compatibility; singleton:=true&#xA;Import-Package: javax.xml.parsers,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.2.0.v20071008&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.boot,org.eclipse.core.internal.boot;x-internal:=true,org.eclipse.core.internal.compatibility;x-internal:=true,org.eclipse.core.internal.model;x-internal:=true,org.eclipse.core.internal.plugins;x-internal:=true,org.eclipse.core.runtime.model&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;;visibility:=reexport,org.eclipse.update.configurator;bundle-version=&quot;[3.1.100,4.0.0)&quot;;visibility:=reexport
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.osgi.util' version='3.1.300.v20080303' singleton='false'>
      <update id='org.eclipse.osgi.util' range='[0.0.0,3.1.300.v20080303)' severity='0'/>
      <properties size='7'>
        <property name='df_LT.osgiUtilDes' value='OSGi Service Platform Release 4.0.1 Utility Classes'/>
        <property name='df_LT.eclipse.org' value='Eclipse.org'/>
        <property name='df_LT.osgiUtil' value='OSGi Release 4.0.1 Utility Classes'/>
        <property name='org.eclipse.equinox.p2.name' value='%osgiUtil'/>
        <property name='org.eclipse.equinox.p2.description' value='%osgiUtilDes'/>
        <property name='org.eclipse.equinox.p2.provider' value='%eclipse.org'/>
        <property name='org.eclipse.equinox.p2.contact' value='www.eclipse.org'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.osgi.util' version='3.1.300.v20080303'/>
        <provided namespace='osgi.bundle' name='org.eclipse.osgi.util' version='3.1.300.v20080303'/>
        <provided namespace='java.package' name='org.osgi.util.measurement' version='1.0.0'/>
        <provided namespace='java.package' name='org.osgi.util.position' version='1.0.0'/>
        <provided namespace='java.package' name='org.osgi.util.xml' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='java.package' name='org.osgi.framework' range='1.1.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.osgi.util' version='3.1.300.v20080303'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %osgiUtil&#xA;Bundle-DocUrl: http://www.eclipse.org&#xA;Bundle-Version: 3.1.300.v20080303&#xA;Import-Package: org.osgi.framework; version=1.1,javax.xml.parsers; resolution:=optional&#xA;Bundle-Vendor: %eclipse.org&#xA;Export-Package: org.osgi.util.measurement; version=&quot;1.0&quot;,org.osgi.util.position; version=&quot;1.0&quot;,org.osgi.util.xml; version=&quot;1.0&quot;&#xA;Bundle-Copyright: %copyright&#xA;Bundle-SymbolicName: org.eclipse.osgi.util&#xA;Manifest-Version: 1.0&#xA;Bundle-Description: %osgiUtilDes&#xA;Bundle-ContactAddress: www.eclipse.org&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: OSGi/Minimum-1.0&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.net.linux.x86' version='1.0.0.I20080521'>
      <update id='org.eclipse.core.net.linux.x86' range='[0.0.0,1.0.0.I20080521)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.fragmentName' value='Proxy for Linux'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%fragmentName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.net.linux.x86' version='1.0.0.I20080521'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.net.linux.x86' version='1.0.0.I20080521'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.core.net' version='1.0.0.I20080521'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.core.net' range='1.1.0'/>
      </requires>
      <filter>
        (&amp; (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.net.linux.x86' version='1.0.0.I20080521'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Fragment-Host: org.eclipse.core.net;bundle-version=&quot;1.1.0&quot;&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 1.0.0.I20080521&#xA;Eclipse-PlatformFilter: (&amp; (osgi.os=linux) (osgi.arch=x86))&#xA;Bundle-SymbolicName: org.eclipse.core.net.linux.x86;singleton:=true&#xA;Bundle-Localization: fragment&#xA;Bundle-Name: %fragmentName&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.update.scheduler' version='3.2.100.v20080404'>
      <update id='org.eclipse.update.scheduler' range='[0.0.0,3.2.100.v20080404)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Automatic Updates Scheduler'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.scheduler' version='3.2.100.v20080404'/>
        <provided namespace='osgi.bundle' name='org.eclipse.update.scheduler' version='3.2.100.v20080404'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.scheduler' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.scheduler.preferences' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.core' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.ui' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='[3.1.100,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.update.scheduler' version='3.2.100.v20080404'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.update.internal.scheduler;x-internal:=true,org.eclipse.update.internal.scheduler.preferences;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.update.core;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.update.ui;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.update.configurator;bundle-version=&quot;[3.1.100,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.update.internal.scheduler.UpdateSchedulerPlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.2.100.v20080404&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.update.scheduler; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.resources' version='3.4.1.R34x_v20080902'>
      <update id='org.eclipse.core.resources' range='[0.0.0,3.4.1.R34x_v20080902)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Core Resource Management'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='19'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.resources' version='3.4.1.R34x_v20080902'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.resources' version='3.4.1.R34x_v20080902'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.dtree' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.localstore' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.properties' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.propertytester' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.refresh' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.resources' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.resources.mapping' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.resources.refresh.win32' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.utils' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.watson' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.resources' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.resources.mapping' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.resources.refresh' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.resources.team' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='osgi.bundle' name='org.eclipse.ant.core' range='[3.1.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.resources' version='3.4.1.R34x_v20080902'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.core.internal.dtree;x-internal:=true,org.eclipse.core.internal.events;x-internal:=true,org.eclipse.core.internal.localstore;x-internal:=true,org.eclipse.core.internal.properties;x-internal:=true,org.eclipse.core.internal.propertytester;x-internal:=true,org.eclipse.core.internal.refresh;x-internal:=true,org.eclipse.core.internal.resources;x-internal:=true,org.eclipse.core.internal.resources.mapping;x-internal:=true,org.eclipse.core.internal.resources.refresh.win32;x-internal:=true,org.eclipse.core.internal.utils;x-internal:=true,org.eclipse.core.internal.watson;x-internal:=true,org.eclipse.core.resources,org.eclipse.core.resources.mapping,org.eclipse.core.resources.refresh,org.eclipse.core.resources.team&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.ant.core;bundle-version=&quot;[3.1.0,4.0.0)&quot;;resolution:=optional,org.eclipse.core.expressions;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.core.resources.ResourcesPlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.4.1.R34x_v20080902&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.resources; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.swt' version='3.4.1.v3449c'>
      <update id='org.eclipse.swt' range='[0.0.0,3.4.1.v3449c)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Standard Widget Toolkit'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='20'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.swt' version='3.4.1.v3449c'/>
        <provided namespace='osgi.bundle' name='org.eclipse.swt' version='3.4.1.v3449c'/>
        <provided namespace='java.package' name='org.eclipse.swt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.accessibility' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.awt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.custom' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.dnd' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.graphics' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.layout' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.opengl' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.printing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.program' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.widgets' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.image' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.swt.internal.theme' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='java.package' name='org.mozilla.xpcom' range='0.0.0' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.swt' version='3.4.1.v3449c'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %pluginName&#xA;Eclipse-ExtensibleAPI: true&#xA;Manifest-Version: 1.0&#xA;DynamicImport-Package: org.mozilla.xpcom&#xA;Bundle-SymbolicName: org.eclipse.swt; singleton:=true&#xA;Bundle-Version: 3.4.1.v3449c&#xA;Export-Package: org.eclipse.swt,org.eclipse.swt.accessibility,org.eclipse.swt.awt,org.eclipse.swt.browser,org.eclipse.swt.custom,org.eclipse.swt.dnd,org.eclipse.swt.events,org.eclipse.swt.graphics,org.eclipse.swt.layout,org.eclipse.swt.opengl,org.eclipse.swt.printing,org.eclipse.swt.program,org.eclipse.swt.widgets,org.eclipse.swt.internal; x-internal:=true,org.eclipse.swt.internal.image; x-internal:=true,org.eclipse.swt.internal.theme; x-internal:=true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.team.core' version='3.4.1.r34x_20080827'>
      <update id='org.eclipse.team.core' range='[0.0.0,3.4.1.r34x_20080827)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Team Support Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='19'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.team.core' version='3.4.1.r34x_20080827'/>
        <provided namespace='osgi.bundle' name='org.eclipse.team.core' version='3.4.1.r34x_20080827'/>
        <provided namespace='java.package' name='org.eclipse.team.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.diff' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.diff.provider' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.history.provider' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.mapping' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.mapping.provider' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.subscribers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.synchronize' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.core.variants' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.core.history' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.core.mapping' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.core.streams' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.team.internal.core.subscribers' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.1.0,2.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.team.core' version='3.4.1.r34x_20080827'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.team.core,org.eclipse.team.core.diff,org.eclipse.team.core.diff.provider,org.eclipse.team.core.history,org.eclipse.team.core.history.provider,org.eclipse.team.core.mapping,org.eclipse.team.core.mapping.provider,org.eclipse.team.core.subscribers,org.eclipse.team.core.synchronize,org.eclipse.team.core.variants,org.eclipse.team.internal.core;x-friends:=&quot;org.eclipse.team.cvs.core,org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui,org.eclipse.team.ui&quot;,org.eclipse.team.internal.core.history;x-friends:=&quot;org.eclipse.team.cvs.core,org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui,org.eclipse.team.ui&quot;,org.eclipse.team.internal.core.mapping;x-friends:=&quot;org.eclipse.team.cvs.core,org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui,org.eclipse.team.ui&quot;,org.eclipse.team.internal.core.streams;x-friends:=&quot;org.eclipse.team.cvs.core,org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui,org.eclipse.team.ui&quot;,org.eclipse.team.internal.core.subscribers;x-friends:=&quot;org.eclipse.team.cvs.core,org.eclipse.team.cvs.ssh,org.eclipse.team.cvs.ssh2,org.eclipse.team.cvs.ui,org.eclipse.team.ui&quot;&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.1.0,2.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.team.internal.core.TeamPlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.4.1.r34x_20080827&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.team.core; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.sat4j.pb' version='2.0.0.v20080602' singleton='false'>
      <update id='org.sat4j.pb' range='[0.0.0,2.0.0.v20080602)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='SAT4J Pseudo'/>
        <property name='df_LT.providerName' value='CRIL CNRS UMR 8188 - Universite d&apos;Artois'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='10'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.sat4j.pb' version='2.0.0.v20080602'/>
        <provided namespace='osgi.bundle' name='org.sat4j.pb' version='2.0.0.v20080602'/>
        <provided namespace='java.package' name='org.sat4j.pb' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.pb.constraints' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.pb.constraints.pb' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.pb.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.pb.orders' version='0.0.0'/>
        <provided namespace='java.package' name='org.sat4j.pb.reader' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.sat4j.core' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.sat4j.pb' version='2.0.0.v20080602'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Implementation-Version: 2.0&#xA;Built-By: Daniel Le Berre&#xA;Specification-Version: NA&#xA;Specification-Title: SAT4J&#xA;Main-Class: org.sat4j.pb.LanceurPseudo2007&#xA;Bundle-Vendor: %providerName&#xA;Export-Package: org.sat4j.pb,org.sat4j.pb.constraints,org.sat4j.pb.constraints.pb,org.sat4j.pb.core,org.sat4j.pb.orders,org.sat4j.pb.reader&#xA;Bundle-Name: %bundleName&#xA;Bundle-Version: 2.0.0.v20080602&#xA;Manifest-Version: 1.0&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Class-Path: org.sat4j.core.jar&#xA;Bundle-ManifestVersion: 2&#xA;Created-By: 10.0-b22 (Sun Microsystems Inc.)&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Implementation-Vendor: CRIL CNRS UMR 8188 - Universite d&apos;Artois&#xA;Implementation-Title: SAT4J&#xA;Bundle-Localization: plugin&#xA;Bundle-SymbolicName: org.sat4j.pb&#xA;Specification-Vendor: Daniel Le Berre&#xA;Require-Bundle: org.sat4j.core
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.ui' version='1.0.1.R34x_v20080909'>
      <update id='org.eclipse.equinox.p2.ui' range='[0.0.0,1.0.1.R34x_v20080909)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Equinox Provisioning UI Support'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='16'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.ui' version='1.0.1.R34x_v20080909'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.ui' version='1.0.1.R34x_v20080909'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui.viewers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.policy' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.query' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.viewers' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='27'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.simpleconfigurator' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.security.ui' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.rollback' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.configurator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository.processing' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine.phases' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.updatechecker' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.ui' version='1.0.1.R34x_v20080909'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Name: %bundleName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.ui.ProvUIActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.ui;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.metadata,org.eclipse.equinox.internal.p2.metadata.repository,org.eclipse.equinox.internal.p2.rollback,org.eclipse.equinox.internal.provisional.configurator,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.artifact.repository.processing,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.engine.phases,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.p2.updatechecker,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.1.R34x_v20080909&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.ui;x-internal:=true,org.eclipse.equinox.internal.p2.ui.dialogs;x-internal:=true,org.eclipse.equinox.internal.p2.ui.model;x-internal:=true,org.eclipse.equinox.internal.p2.ui.viewers;x-internal:=true,org.eclipse.equinox.internal.provisional.p2.ui; x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.ui.actions; x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.ui.dialogs; x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.ui.model;x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,org.eclipse.equinox.p2.ui.admin.rcp,org.eclipse.equinox.p2.ui.sdk&quot;,org.eclipse.equinox.internal.provisional.p2.ui.operations; x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.ui.policy;x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,org.eclipse.equinox.p2.ui.admin.rcp,org.eclipse.equinox.p2.ui.sdk&quot;,org.eclipse.equinox.internal.provisional.p2.ui.query;x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,org.eclipse.equinox.p2.ui.admin.rcp,org.eclipse.equinox.p2.ui.sdk&quot;,org.eclipse.equinox.internal.provisional.p2.ui.viewers;x-friends:=&quot;org.eclipse.equinox.p2.ui.admin,org.eclipse.equinox.p2.ui.admin.rcp,org.eclipse.equinox.p2.ui.sdk&quot;&#xA;Require-Bundle: org.eclipse.ui,org.eclipse.core.runtime,org.eclipse.equinox.simpleconfigurator,org.eclipse.equinox.security.ui;bundle-version=&quot;[1.0.0,2.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.ide' version='3.4.1.M20080903-2000'>
      <update id='org.eclipse.ui.ide' range='[0.0.0,3.4.1.M20080903-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Eclipse IDE UI'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='41'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.ide' version='3.4.1.M20080903-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.ide' version='3.4.1.M20080903-2000'/>
        <provided namespace='java.package' name='org.eclipse.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.ide' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.ide.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.ide.fileSystem' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.ide.undo' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.filesystem' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.handlers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.misc' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.registry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.undo' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.ide.update' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.bookmarkexplorer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.framelist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.markers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.navigator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.properties' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.views.tasklist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.wizards.datatransfer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.wizards.newresource' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.part' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.bookmarkexplorer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.framelist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.markers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.markers.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.navigator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.properties' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views.tasklist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.wizards.datatransfer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.wizards.newresource' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='12'>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.filesystem' range='[1.0.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.views' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='[3.1.100,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.core' range='[3.1.100,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.ui' range='[3.1.100,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.3.0,4.0.0)' optional='true'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.ide' version='3.4.1.M20080903-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %Plugin.name&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 3.4.1.M20080903-2000&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-Activator: org.eclipse.ui.internal.ide.IDEWorkbenchPlugin&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Require-Bundle: org.eclipse.core.resources;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.filesystem;bundle-version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui.views;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.update.configurator;bundle-version=&quot;[3.1.100,4.0.0)&quot;;resolution:=optional,org.eclipse.update.core;bundle-version=&quot;[3.1.100,4.0.0)&quot;;resolution:=optional,org.eclipse.update.ui;bundle-version=&quot;[3.1.100,4.0.0)&quot;;resolution:=optional,org.eclipse.jface.text;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.3.0,4.0.0)&quot;;resolution:=optional&#xA;Export-Package: org.eclipse.ui,org.eclipse.ui.actions,org.eclipse.ui.dialogs,org.eclipse.ui.ide,org.eclipse.ui.ide.dialogs,org.eclipse.ui.ide.fileSystem,org.eclipse.ui.ide.undo,org.eclipse.ui.internal.ide;x-friends:=&quot;org.eclipse.ui.ide.application&quot;,org.eclipse.ui.internal.ide.actions;x-internal:=true,org.eclipse.ui.internal.ide.commands;x-internal:=true,org.eclipse.ui.internal.ide.dialogs;x-friends:=&quot;org.eclipse.ui.ide.application&quot;,org.eclipse.ui.internal.ide.filesystem;x-internal:=true,org.eclipse.ui.internal.ide.handlers;x-internal:=true,org.eclipse.ui.internal.ide.misc;x-internal:=true,org.eclipse.ui.internal.ide.model;x-friends:=&quot;org.eclipse.ui.ide.application&quot;,org.eclipse.ui.internal.ide.registry;x-internal:=true,org.eclipse.ui.internal.ide.undo;x-friends:=&quot;org.eclipse.ui.ide.application&quot;,org.eclipse.ui.internal.ide.update;x-internal:=true,org.eclipse.ui.internal.views.bookmarkexplorer;x-internal:=true,org.eclipse.ui.internal.views.framelist;x-internal:=true,org.eclipse.ui.internal.views.markers;x-internal:=true,org.eclipse.ui.internal.views.navigator;x-internal:=true,org.eclipse.ui.internal.views.properties;x-internal:=true,org.eclipse.ui.internal.views.tasklist;x-internal:=true,org.eclipse.ui.internal.wizards.datatransfer;x-internal:=true,org.eclipse.ui.internal.wizards.newresource;x-internal:=true,org.eclipse.ui.model,org.eclipse.ui.part,org.eclipse.ui.views.bookmarkexplorer,org.eclipse.ui.views.framelist,org.eclipse.ui.views.markers,org.eclipse.ui.views.markers.internal;x-internal:=true,org.eclipse.ui.views.navigator,org.eclipse.ui.views.properties,org.eclipse.ui.views.tasklist,org.eclipse.ui.wizards.datatransfer,org.eclipse.ui.wizards.newresource&#xA;Bundle-SymbolicName: org.eclipse.ui.ide; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.frameworkadmin' version='1.0.2.R34x_v20080910'>
      <update id='org.eclipse.equinox.frameworkadmin' range='[0.0.0,1.0.2.R34x_v20080910)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Framework Admin'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.frameworkadmin' version='1.0.2.R34x_v20080910'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.frameworkadmin' version='1.0.2.R34x_v20080910'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.utils' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.configuratormanipulator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.frameworkadmin' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='3.4.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.pluginconversion' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.startlevel' range='1.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.frameworkadmin' version='1.0.2.R34x_v20080910'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.frameworkadmin.utils.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.equinox.frameworkadmin;singleton:=true&#xA;Import-Package: org.eclipse.osgi.service.pluginconversion;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.startlevel;version=&quot;1.0.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.0.2.R34x_v20080910&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.frameworkadmin.utils;x-friends:=&quot;org.eclipse.equinox.frameworkadmin.equinox,org.eclipse.equinox.p2.metadata.generator&quot;,org.eclipse.equinox.internal.provisional.configuratormanipulator;x-internal:=true,org.eclipse.equinox.internal.provisional.frameworkadmin;x-friends:=&quot;org.eclipse.pde.p2.ui,org.eclipse.equinox.p2.touchpoint.eclipse,org.eclipse.equinox.p2.metadata.generator&quot;&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;3.4.0&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolingorg.eclipse.platform.ide.ini.gtk.linux.x86' version='3.4.0.M20080911-1700'>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.ini.gtk.linux.x86' version='3.4.0.M20080911-1700'/>
        <provided namespace='toolingorg.eclipse.platform.ide' name='org.eclipse.platform.ide.ini' version='3.4.0.M20080911-1700'/>
      </provides>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='configure'>
            addJvmArg(jvmArg:-Xms40m);addJvmArg(jvmArg:-Xmx256m);addProgramArg(programArg:-showsplash);addProgramArg(programArg:org.eclipse.platform);addProgramArg(programArg:--launcher.XXMaxPermSize);addProgramArg(programArg:256m);
          </instruction>
          <instruction key='unconfigure'>
            removeJvmArg(jvmArg:-Xms40m);removeJvmArg(jvmArg:-Xmx256m);removeProgramArg(programArg:-showsplash);removeProgramArg(programArg:org.eclipse.platform);removeProgramArg(programArg:--launcher.XXMaxPermSize);removeProgramArg(programArg:256m);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='tooling.source.default' version='1.0.0' singleton='false'>
      <hostRequirements size='1'>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='source' range='0.0.0' optional='true' multiple='true' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='tooling.source.default' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='tooling' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='source' range='0.0.0' optional='true' multiple='true' greedy='false'/>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='uninstall'>
            removeSourceBundle(bundle:${artifact})
          </instruction>
          <instruction key='install'>
            addSourceBundle(bundle:${artifact})
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ecf.provider.filetransfer' version='2.0.0.v20080611-1715'>
      <update id='org.eclipse.ecf.provider.filetransfer' range='[0.0.0,2.0.0.v20080611-1715)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin.name' value='ECF Filetransfer Provider'/>
        <property name='df_LT.plugin.provider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%plugin.provider'/>
      </properties>
      <provides size='10'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.provider.filetransfer' version='2.0.0.v20080611-1715'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ecf.provider.filetransfer' version='2.0.0.v20080611-1715'/>
        <provided namespace='java.package' name='org.eclipse.ecf.internal.provider.filetransfer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.provider.filetransfer.browse' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.provider.filetransfer.identity' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.provider.filetransfer.outgoing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.provider.filetransfer.retrieve' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.provider.filetransfer.util' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf.filetransfer' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.net.proxy' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.jobs' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.log' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.url' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.2'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ecf.provider.filetransfer' version='2.0.0.v20080611-1715'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Version: 2.0.0.v20080611-1715&#xA;Bundle-ActivationPolicy: lazy&#xA;Eclipse-LazyStart: true&#xA;Export-Package: org.eclipse.ecf.internal.provider.filetransfer;x-internal:=true,org.eclipse.ecf.provider.filetransfer.browse,org.eclipse.ecf.provider.filetransfer.identity;x-internal:=false,org.eclipse.ecf.provider.filetransfer.outgoing,org.eclipse.ecf.provider.filetransfer.retrieve;x-internal:=false,org.eclipse.ecf.provider.filetransfer.util&#xA;Import-Package: org.eclipse.core.net.proxy;resolution:=optional,org.eclipse.core.runtime.jobs,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.log;version=&quot;1.3.0&quot;,org.osgi.service.url;version=&quot;1.0.0&quot;,org.osgi.util.tracker;version=&quot;1.3.2&quot;&#xA;Manifest-Version: 1.0&#xA;Bundle-Activator: org.eclipse.ecf.internal.provider.filetransfer.Activator&#xA;Bundle-ClassPath: .&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.ecf,org.eclipse.ecf.filetransfer,org.eclipse.equinox.registry&#xA;Bundle-SymbolicName: org.eclipse.ecf.provider.filetransfer;singleton:=true&#xA;Created-By: 10.0-b19 (Sun Microsystems Inc.)&#xA;Bundle-Vendor: %plugin.provider&#xA;Bundle-Name: %plugin.name&#xA;Bundle-ManifestVersion: 2
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolinggtk.linux.x86org.eclipse.update.configurator' version='3.2.201.R34x_v20080819' singleton='false'>
      <hostRequirements size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='3.2.201.R34x_v20080819'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolinggtk.linux.x86org.eclipse.update.configurator' version='3.2.201.R34x_v20080819'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='toolinggtk.linux.x86' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='3.2.201.R34x_v20080819'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            setProgramProperty(propName:org.eclipse.update.reconcile, propValue:false);
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>
            setProgramProperty(propName:org.eclipse.update.reconcile, propValue:);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ecf.ssl' version='1.0.0.v20080611-1715' singleton='false'>
      <update id='org.eclipse.ecf.ssl' range='[0.0.0,1.0.0.v20080611-1715)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin.name' value='ECF SSL Fragment'/>
        <property name='df_LT.plugin.provider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%plugin.provider'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.ssl' version='1.0.0.v20080611-1715'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ecf.ssl' version='1.0.0.v20080611-1715'/>
        <provided namespace='java.package' name='org.eclipse.ecf.internal.ssl' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.ecf' version='1.0.0.v20080611-1715'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.ecf' range='2.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.security' range='1.0.0'/>
        <required namespace='java.package' name='javax.net.ssl' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ecf.ssl' version='1.0.0.v20080611-1715'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Bundle-Name: %plugin.name&#xA;Created-By: 10.0-b19 (Sun Microsystems Inc.)&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %plugin.provider&#xA;Bundle-SymbolicName: org.eclipse.ecf.ssl&#xA;Import-Package: org.eclipse.osgi.service.security;version=&quot;1.0.0&quot;,javax.net.ssl&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.0.0.v20080611-1715&#xA;Bundle-Localization: plugin&#xA;Fragment-Host: org.eclipse.ecf;bundle-version=&quot;2.0.0&quot;&#xA;Export-Package: org.eclipse.ecf.internal.ssl;x-internal:=true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.ui.sdk' version='1.0.1.R34x_v20080818'>
      <update id='org.eclipse.equinox.p2.ui.sdk' range='[0.0.0,1.0.1.R34x_v20080818)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Equinox Provisioning Platform Update Support'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.ui.sdk' version='1.0.1.R34x_v20080818'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.ui.sdk' version='1.0.1.R34x_v20080818'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui.sdk' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui.sdk.prefs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.ui.sdk.updates' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.sdk' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='27'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.generator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.actions' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.dialogs' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.model' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.operations' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.policy' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.ui.viewers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.updatechecker' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.ui.sdk' version='1.0.1.R34x_v20080818'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Name: %bundleName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.ui.sdk.ProvSDKUIActivator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.ui.sdk;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.generator,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.p2.ui,org.eclipse.equinox.internal.provisional.p2.ui.actions,org.eclipse.equinox.internal.provisional.p2.ui.dialogs,org.eclipse.equinox.internal.provisional.p2.ui.model,org.eclipse.equinox.internal.provisional.p2.ui.operations,org.eclipse.equinox.internal.provisional.p2.ui.policy,org.eclipse.equinox.internal.provisional.p2.ui.query,org.eclipse.equinox.internal.provisional.p2.ui.viewers,org.eclipse.equinox.internal.provisional.p2.updatechecker,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.1.R34x_v20080818&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.ui.sdk;x-internal:=true,org.eclipse.equinox.internal.p2.ui.sdk.prefs;x-internal:=true,org.eclipse.equinox.internal.p2.ui.sdk.updates;x-internal:=true,org.eclipse.equinox.internal.provisional.p2.ui.sdk;x-friends:=&quot;org.eclipse.pde.p2.ui&quot;&#xA;Require-Bundle: org.eclipse.ui,org.eclipse.core.runtime
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.compare' version='3.4.0.I20080604'>
      <update id='org.eclipse.compare' range='[0.0.0,3.4.0.I20080604)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Compare Support'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='12'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.compare' version='3.4.0.I20080604'/>
        <provided namespace='osgi.bundle' name='org.eclipse.compare' version='3.4.0.I20080604'/>
        <provided namespace='java.package' name='org.eclipse.compare' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.contentmergeviewer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.internal.merge' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.internal.patch' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.patch' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.rangedifferencer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.compare.structuremergeviewer' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='12'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.resources' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.views' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.editors' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.util' range='0.0.0'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.compare' version='3.4.0.I20080604'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.compare.internal.CompareUIPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.compare; singleton:=true&#xA;Import-Package: com.ibm.icu.util,com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.0.I20080604&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.compare,org.eclipse.compare.contentmergeviewer,org.eclipse.compare.internal;x-internal:=true,org.eclipse.compare.internal.merge;x-internal:=true,org.eclipse.compare.internal.patch;x-internal:=true,org.eclipse.compare.patch,org.eclipse.compare.rangedifferencer,org.eclipse.compare.structuremergeviewer&#xA;Require-Bundle: org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.resources;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.ide;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.views;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.workbench.texteditor;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.editors;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolingorg.eclipse.equinox.launcher' version='1.0.101.R34x_v20080819' singleton='false'>
      <hostRequirements size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.launcher' range='1.0.101.R34x_v20080819'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='2'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.equinox.launcher' version='1.0.101.R34x_v20080819'/>
        <provided namespace='org.eclipse.equinox.p2.flavor' name='tooling' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.launcher' range='1.0.101.R34x_v20080819'/>
        <required namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' range='[1.0.0,2.0.0)' greedy='false'/>
      </requires>
      <touchpoint id='null' version='0.0.0'/>
      <touchpointData size='1'>
        <instructions size='4'>
          <instruction key='uninstall'>
            uninstallBundle(bundle:${artifact})
          </instruction>
          <instruction key='configure'>
            addProgramArg(programArg:-startup);addProgramArg(programArg:@artifact);
          </instruction>
          <instruction key='install'>
            installBundle(bundle:${artifact})
          </instruction>
          <instruction key='unconfigure'>
            removeProgramArg(programArg:-startup);removeProgramArg(programArg:@artifact);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.touchpoint.natives' version='1.0.0.v20080505-1850'>
      <update id='org.eclipse.equinox.p2.touchpoint.natives' range='[0.0.0,1.0.0.v20080505-1850)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Native Touchpoint'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.touchpoint.natives' version='1.0.0.v20080505-1850'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.touchpoint.natives' version='1.0.0.v20080505-1850'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.touchpoint.natives' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='13'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.3'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.touchpoint.natives' version='1.0.0.v20080505-1850'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.touchpoint.natives.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.touchpoint.natives;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.engine,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.location,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.0.v20080505-1850&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.touchpoint.natives;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.frameworkadmin.equinox' version='1.0.2.R34x_v20080911'>
      <update id='org.eclipse.equinox.frameworkadmin.equinox' range='[0.0.0,1.0.2.R34x_v20080911)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Framework Admin for Equinox'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.frameworkadmin.equinox' version='1.0.2.R34x_v20080911'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.frameworkadmin.equinox' version='1.0.2.R34x_v20080911'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.equinox' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.equinox.utils' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='18'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.adaptor' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.internal.adaptor' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.utils' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.configuratormanipulator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.frameworkadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.adaptor' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.debug' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.internal.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.storagemanager' range='1.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.log' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.startlevel' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.2'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.frameworkadmin.equinox' version='1.0.2.R34x_v20080911'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.frameworkadmin.equinox;x-friends:=org.eclipse.equinox.p2.metadata.generator,org.eclipse.equinox.internal.frameworkadmin.equinox.utils;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common&#xA;Bundle-Activator: org.eclipse.equinox.internal.frameworkadmin.equinox.Activator&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.2.R34x_v20080911&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.frameworkadmin.equinox;singleton:=true&#xA;Import-Package: org.eclipse.core.runtime.adaptor,org.eclipse.core.runtime.internal.adaptor,org.eclipse.equinox.internal.frameworkadmin.utils,org.eclipse.equinox.internal.provisional.configuratormanipulator,org.eclipse.equinox.internal.provisional.frameworkadmin,org.eclipse.osgi.framework.adaptor;resolution:=optional,org.eclipse.osgi.framework.debug;resolution:=optional,org.eclipse.osgi.framework.internal.core,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.osgi.service.environment;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;;resolution:=optional,org.eclipse.osgi.storagemanager;version=&quot;1.0.0&quot;;resolution:=optional,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.log;version=&quot;1.3.0&quot;,org.osgi.service.startlevel;version=&quot;1.0.0&quot;,org.osgi.util.tracker;version=&quot;1.3.2&quot;&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help.feature.jar' version='1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat'>
      <update id='org.eclipse.help.feature.jar' range='[0.0.0,1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat)' severity='0'/>
      <properties size='9'>
        <property name='org.eclipse.equinox.p2.name' value='%featureName'/>
        <property name='org.eclipse.equinox.p2.description' value='%description'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
        <property name='org.eclipse.update.feature.plugin' value='org.eclipse.help.base'/>
        <property name='df_LT.featureName' value='Eclipse Help System'/>
        <property name='df_LT.copyright' value='Copyright (c) 2008 IBM Corporation and others.&#xA;All rights reserved. This program and the accompanying materials&#xA;are made available under the terms of the Eclipse Public License v1.0&#xA;which accompanies this distribution, and is available at&#xA;http://www.eclipse.org/legal/epl-v10.html&#xA;&#xA;Contributors:&#xA;IBM Corporation - initial API and implementation'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.description' value='Eclipse help system.'/>
        <property name='df_LT.license' value='ECLIPSE FOUNDATION SOFTWARE USER AGREEMENT&#xA;March 17, 2005&#xA;&#xA;Usage Of Content&#xA;&#xA;THE ECLIPSE FOUNDATION MAKES AVAILABLE SOFTWARE, DOCUMENTATION, INFORMATION AND/OR&#xA;OTHER MATERIALS FOR OPEN SOURCE PROJECTS (COLLECTIVELY &quot;CONTENT&quot;).&#xA;USE OF THE CONTENT IS GOVERNED BY THE TERMS AND CONDITIONS OF THIS&#xA;AGREEMENT AND/OR THE TERMS AND CONDITIONS OF LICENSE AGREEMENTS OR&#xA;NOTICES INDICATED OR REFERENCED BELOW. BY USING THE CONTENT, YOU&#xA;AGREE THAT YOUR USE OF THE CONTENT IS GOVERNED BY THIS AGREEMENT&#xA;AND/OR THE TERMS AND CONDITIONS OF ANY APPLICABLE LICENSE AGREEMENTS&#xA;OR NOTICES INDICATED OR REFERENCED BELOW. IF YOU DO NOT AGREE TO THE&#xA;TERMS AND CONDITIONS OF THIS AGREEMENT AND THE TERMS AND CONDITIONS&#xA;OF ANY APPLICABLE LICENSE AGREEMENTS OR NOTICES INDICATED OR REFERENCED&#xA;BELOW, THEN YOU MAY NOT USE THE CONTENT.&#xA;&#xA;Applicable Licenses&#xA;&#xA;Unless otherwise indicated, all Content made available by the Eclipse Foundation&#xA;is provided to you under the terms and conditions of the Eclipse Public&#xA;License Version 1.0 (&quot;EPL&quot;). A copy of the EPL is provided with this&#xA;Content and is also available at http://www.eclipse.org/legal/epl-v10.html.&#xA;For purposes of the EPL, &quot;Program&quot; will mean the Content.&#xA;&#xA;Content includes, but is not limited to, source code, object code,&#xA;documentation and other files maintained in the Eclipse.org CVS&#xA;repository (&quot;Repository&quot;) in CVS modules (&quot;Modules&quot;) and made available&#xA;as downloadable archives (&quot;Downloads&quot;).&#xA;&#xA;- Content may be structured and packaged into modules to facilitate delivering,&#xA;extending, and upgrading the Content. Typical modules may include plug-ins (&quot;Plug-ins&quot;),&#xA;plug-in fragments (&quot;Fragments&quot;), and features (&quot;Features&quot;).&#xA;- Each Plug-in or Fragment may be packaged as a sub-directory or JAR (Java? ARchive)&#xA;in a directory named &quot;plugins&quot;.&#xA;- A Feature is a bundle of one or more Plug-ins and/or Fragments and associated material.&#xA;Each Feature may be packaged as a sub-directory in a directory named &quot;features&quot;.&#xA;Within a Feature, files named &quot;feature.xml&quot; may contain a list of the names and version&#xA;numbers of the Plug-ins and/or Fragments associated with that Feature.&#xA;- Features may also include other Features (&quot;Included Features&quot;). Within a Feature, files&#xA;named &quot;feature.xml&quot; may contain a list of the names and version numbers of Included Features.&#xA;&#xA;Features may also include other Features (&quot;Included Features&quot;). Files named&#xA;&quot;feature.xml&quot; may contain a list of the names and version numbers of&#xA;Included Features.&#xA;&#xA;The terms and conditions governing Plug-ins and Fragments should be&#xA;contained in files named &quot;about.html&quot; (&quot;Abouts&quot;). The terms and&#xA;conditions governing Features and Included Features should be contained&#xA;in files named &quot;license.html&quot; (&quot;Feature Licenses&quot;). Abouts and Feature&#xA;Licenses may be located in any directory of a Download or Module&#xA;including, but not limited to the following locations:&#xA;&#xA;- The top-level (root) directory&#xA;- Plug-in and Fragment directories&#xA;- Inside Plug-ins and Fragments packaged as JARs&#xA;- Sub-directories of the directory named &quot;src&quot; of certain Plug-ins&#xA;- Feature directories&#xA;&#xA;Note: if a Feature made available by the Eclipse Foundation is installed using the&#xA;Eclipse Update Manager, you must agree to a license (&quot;Feature Update&#xA;License&quot;) during the installation process. If the Feature contains&#xA;Included Features, the Feature Update License should either provide you&#xA;with the terms and conditions governing the Included Features or inform&#xA;you where you can locate them. Feature Update Licenses may be found in&#xA;the &quot;license&quot; property of files named &quot;feature.properties&quot;. Such Abouts,&#xA;Feature Licenses and Feature Update Licenses contain the terms and&#xA;conditions (or references to such terms and conditions) that govern your&#xA;use of the associated Content in that directory.&#xA;&#xA;THE ABOUTS, FEATURE LICENSES AND FEATURE UPDATE LICENSES MAY REFER&#xA;TO THE EPL OR OTHER LICENSE AGREEMENTS, NOTICES OR TERMS AND CONDITIONS.&#xA;SOME OF THESE OTHER LICENSE AGREEMENTS MAY INCLUDE (BUT ARE NOT LIMITED TO):&#xA;&#xA;- Common Public License Version 1.0 (available at http://www.eclipse.org/legal/cpl-v10.html)&#xA;- Apache Software License 1.1 (available at http://www.apache.org/licenses/LICENSE)&#xA;- Apache Software License 2.0 (available at http://www.apache.org/licenses/LICENSE-2.0)&#xA;- IBM Public License 1.0 (available at http://oss.software.ibm.com/developerworks/opensource/license10.html)&#xA;- Metro Link Public License 1.00 (available at http://www.opengroup.org/openmotif/supporters/metrolink/license.html)&#xA;- Mozilla Public License Version 1.1 (available at http://www.mozilla.org/MPL/MPL-1.1.html)&#xA;&#xA;IT IS YOUR OBLIGATION TO READ AND ACCEPT ALL SUCH TERMS AND CONDITIONS PRIOR&#xA;TO USE OF THE CONTENT. If no About, Feature License or Feature Update License&#xA;is provided, please contact the Eclipse Foundation to determine what terms and conditions&#xA;govern that particular Content.&#xA;&#xA;Cryptography&#xA;&#xA;Content may contain encryption software. The country in which you are&#xA;currently may have restrictions on the import, possession, and use,&#xA;and/or re-export to another country, of encryption software. BEFORE&#xA;using any encryption software, please check the country&apos;s laws,&#xA;regulations and policies concerning the import, possession, or use,&#xA;and re-export of encryption software, to see if this is permitted.&#xA;&#xA;Java and all Java-based trademarks are trademarks of Sun Microsystems, Inc. in the United States, other countries, or both.'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.feature.jar' version='1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='feature' version='1.0.0'/>
        <provided namespace='org.eclipse.update.feature' name='org.eclipse.help' version='1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <filter>
        (org.eclipse.update.install.features=true)
      </filter>
      <artifacts size='1'>
        <artifact classifier='org.eclipse.update.feature' id='org.eclipse.help' version='1.0.1.R34x_v20080827-7r7xEIxEI6Zu5nEqN7M3UBpglaat'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
      <licenses size='1'>
        <license>
          %license
        </license>
      </licenses>
      <copyright>
        %copyright
      </copyright>
    </unit>
    <unit id='com.jcraft.jsch' version='0.1.37.v200803061811' singleton='false'>
      <update id='com.jcraft.jsch' range='[0.0.0,0.1.37.v200803061811)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='JSch'/>
        <property name='df_LT.venderName' value='JCraft, Inc.'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%venderName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='com.jcraft.jsch' version='0.1.37.v200803061811'/>
        <provided namespace='osgi.bundle' name='com.jcraft.jsch' version='0.1.37.v200803061811'/>
        <provided namespace='java.package' name='com.jcraft.jsch' version='0.0.0'/>
        <provided namespace='java.package' name='com.jcraft.jsch.jce' version='0.0.0'/>
        <provided namespace='java.package' name='com.jcraft.jsch.jcraft' version='0.0.0'/>
        <provided namespace='java.package' name='com.jcraft.jsch.jgss' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='com.jcraft.jsch' version='0.1.37.v200803061811'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 0.1.37.v200803061811&#xA;Export-Package: com.jcraft.jsch,com.jcraft.jsch.jce;x-internal:=true,com.jcraft.jsch.jcraft;x-internal:=true,com.jcraft.jsch.jgss;x-internal:=true&#xA;Bundle-SymbolicName: com.jcraft.jsch&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %bundleName&#xA;Bundle-ClassPath: .&#xA;Bundle-Vendor: %venderName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ecf' version='2.0.0.v20080611-1715'>
      <update id='org.eclipse.ecf' range='[0.0.0,2.0.0.v20080611-1715)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin.name' value='Eclipse Communication Framework (ECF)'/>
        <property name='df_LT.plugin.provider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%plugin.provider'/>
      </properties>
      <provides size='12'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf' version='2.0.0.v20080611-1715'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ecf' version='2.0.0.v20080611-1715'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.provider' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.security' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.start' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.user' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.core.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ecf.internal.core' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf.identity' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.jobs' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.log' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.2'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ecf' version='2.0.0.v20080611-1715'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Import-Package: org.eclipse.core.runtime.jobs,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.log;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.2&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Created-By: 10.0-b19 (Sun Microsystems Inc.)&#xA;Manifest-Version: 1.0&#xA;Bundle-Name: %plugin.name&#xA;Bundle-Vendor: %plugin.provider&#xA;Bundle-ActivationPolicy: lazy&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Bundle-Version: 2.0.0.v20080611-1715&#xA;Export-Package: org.eclipse.ecf.core,org.eclipse.ecf.core.events,org.eclipse.ecf.core.provider,org.eclipse.ecf.core.security,org.eclipse.ecf.core.start,org.eclipse.ecf.core.user,org.eclipse.ecf.core.util,org.eclipse.ecf.internal.core;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry,org.eclipse.ecf.identity;visibility:=reexport&#xA;Bundle-Activator: org.eclipse.ecf.internal.core.ECFPlugin&#xA;Bundle-SymbolicName: org.eclipse.ecf;singleton:=true&#xA;Eclipse-LazyStart: true&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help.webapp' version='3.3.101.M20080805_34x'>
      <update id='org.eclipse.help.webapp' range='[0.0.0,3.3.101.M20080805_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.help_webapp_plugin_name' value='Help System Webapp'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%help_webapp_plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.webapp' version='3.3.101.M20080805_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.help.webapp' version='3.3.101.M20080805_34x'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.webapp' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.webapp.data' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.webapp.servlet' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.help.base' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.apache.jasper' range='5.5.17'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.jsp.jasper.registry' range='1.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.http.registry' range='1.0.0'/>
        <required namespace='java.package' name='javax.servlet' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.4.0'/>
        <required namespace='java.package' name='org.osgi.service.http' range='1.2.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.help.webapp' version='3.3.101.M20080805_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %help_webapp_plugin_name&#xA;Bundle-Activator: org.eclipse.help.internal.webapp.HelpWebappPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.help.webapp;singleton:=true&#xA;Import-Package: javax.servlet;version=&quot;2.4.0&quot;,javax.servlet.http;version=&quot;2.4.0&quot;,org.osgi.service.http;version=&quot;1.2.0&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.3.101.M20080805_34x&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.help.internal.webapp;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.internal.webapp.data;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.internal.webapp.servlet;x-friends:=&quot;org.eclipse.ua.tests&quot;&#xA;Require-Bundle: org.eclipse.help.base;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.apache.jasper;bundle-version=&quot;5.5.17&quot;,org.eclipse.equinox.jsp.jasper.registry;bundle-version=&quot;1.0.0&quot;,org.eclipse.equinox.http.registry;bundle-version=&quot;1.0.0&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.runtime.compatibility.auth' version='3.2.100.v20070502' singleton='false'>
      <update id='org.eclipse.core.runtime.compatibility.auth' range='[0.0.0,3.2.100.v20070502)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Authorization Compatibility Plug-in'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.runtime.compatibility.auth' version='3.2.100.v20070502'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.runtime.compatibility.auth' version='3.2.100.v20070502'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.runtime.auth' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='6'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.runtime.compatibility.auth' version='3.2.100.v20070502'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.core.internal.runtime.auth.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.core.runtime.compatibility.auth&#xA;Import-Package: org.eclipse.osgi.framework.log,org.eclipse.osgi.service.datalocation,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework,org.osgi.util.tracker&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.2.100.v20070502&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.internal.runtime.auth;x-friends:=&quot;org.eclipse.core.runtime&quot;&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.boot' version='3.1.100.v20080218' singleton='false'>
      <update id='org.eclipse.core.boot' range='[0.0.0,3.1.100.v20080218)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Core Boot'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.boot' version='3.1.100.v20080218'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.boot' version='3.1.100.v20080218'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.boot' version='3.1.100.v20080218'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Eclipse-AutoStart: true&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.1.100.v20080218&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-SymbolicName: org.eclipse.core.boot&#xA;Bundle-Localization: plugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ecf.provider.filetransfer.ssl' version='1.0.0.v20080611-1715' singleton='false'>
      <update id='org.eclipse.ecf.provider.filetransfer.ssl' range='[0.0.0,1.0.0.v20080611-1715)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.plugin.name' value='ECF Filetransfer SSL Fragment'/>
        <property name='df_LT.plugin.provider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%plugin.provider'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ecf.provider.filetransfer.ssl' version='1.0.0.v20080611-1715'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ecf.provider.filetransfer.ssl' version='1.0.0.v20080611-1715'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='osgi.fragment' name='org.eclipse.ecf.provider.filetransfer' version='1.0.0.v20080611-1715'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.ecf.provider.filetransfer' range='2.0.0'/>
        <required namespace='java.package' name='javax.net.ssl' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ecf.provider.filetransfer.ssl' version='1.0.0.v20080611-1715'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %plugin.name&#xA;Bundle-Version: 1.0.0.v20080611-1715&#xA;Bundle-Vendor: %plugin.provider&#xA;Bundle-SymbolicName: org.eclipse.ecf.provider.filetransfer.ssl&#xA;Import-Package: javax.net.ssl&#xA;Fragment-Host: org.eclipse.ecf.provider.filetransfer;bundle-version=&quot;2.0.0&quot;&#xA;Created-By: 10.0-b19 (Sun Microsystems Inc.)&#xA;Ant-Version: Apache Ant 1.7.0&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help.base' version='3.3.101.M20080728_34x'>
      <update id='org.eclipse.help.base' range='[0.0.0,3.3.101.M20080728_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.help_base_plugin_name' value='Help System Base'/>
        <property name='org.eclipse.equinox.p2.name' value='%help_base_plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='21'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help.base' version='3.3.101.M20080728_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.help.base' version='3.3.101.M20080728_34x'/>
        <provided namespace='java.package' name='org.apache.lucene.demo.html' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.base' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.base.remote' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.base.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.browser.macosx' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.protocols' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.search' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.search.federated' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.server' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.standalone' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.validation' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.workingset' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.xhtml' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.search' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.standalone' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.apache.lucene' range='[1.9.1,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.apache.lucene.analysis' range='[1.9.1,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ant.core' range='3.1.0' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='3.3.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.http.jetty' range='0.0.0' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.help.base' version='3.3.101.M20080728_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %help_base_plugin_name&#xA;Bundle-Activator: org.eclipse.help.internal.base.HelpBasePlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.help.base; singleton:=true&#xA;Import-Package: com.ibm.icu.text,org.eclipse.equinox.http.jetty;resolution:=optional&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.3.101.M20080728_34x&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.apache.lucene.demo.html;x-internal:=true,org.eclipse.help.browser,org.eclipse.help.internal.base;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.help.webapp,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.base.remote;x-friends:=&quot;org.eclipse.ua.tests,org.eclipse.help.webapp,org.eclipse.help.ui&quot;,org.eclipse.help.internal.base.util;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.help.webapp,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.browser;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.browser.macosx;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.internal.protocols;x-friends:=&quot;org.eclipse.help.base,org.eclipse.help.ui,org.eclipse.help.webapp,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.search;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.help.webapp,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.search.federated;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.server;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.internal.standalone;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.internal.validation;x-friends:=&quot;org.eclipse.ua.tests&quot;,org.eclipse.help.internal.workingset;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.help.webapp,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.xhtml;x-friends:=&quot;org.eclipse.help.ui,org.eclipse.ua.tests,org.eclipse.ui.intro,org.eclipse.help.webapp&quot;,org.eclipse.help.search,org.eclipse.help.standalone&#xA;Require-Bundle: org.apache.lucene;bundle-version=&quot;[1.9.1,2.0.0)&quot;;visibility:=reexport,org.apache.lucene.analysis;bundle-version=&quot;[1.9.1,2.0.0)&quot;;visibility:=reexport,org.eclipse.ant.core;bundle-version=&quot;3.1.0&quot;;resolution:=optional,org.eclipse.core.runtime;bundle-version=&quot;3.3.0&quot;,org.eclipse.help;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.jsp.jasper.registry' version='1.0.0.v20080427-0830' singleton='false'>
      <update id='org.eclipse.equinox.jsp.jasper.registry' range='[0.0.0,1.0.0.v20080427-0830)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Jasper Jsp Registry Support Plug-in'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.jsp.jasper.registry' version='1.0.0.v20080427-0830'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.jsp.jasper.registry' version='1.0.0.v20080427-0830'/>
        <provided namespace='java.package' name='org.eclipse.equinox.jsp.jasper.registry' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.jsp.jasper' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.0'/>
        <required namespace='java.package' name='javax.servlet' range='2.4.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.4.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.jsp.jasper.registry' version='1.0.0.v20080427-0830'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-Name: %bundleName&#xA;Require-Bundle: org.eclipse.equinox.registry,org.eclipse.equinox.common&#xA;Bundle-Activator: org.eclipse.equinox.internal.jsp.jasper.registry.Activator&#xA;Bundle-Vendor: %providerName&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 1.0.0.v20080427-0830&#xA;Bundle-Localization: plugin&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Export-Package: org.eclipse.equinox.jsp.jasper.registry;version=&quot;1.0.0&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.jsp.jasper.registry&#xA;Import-Package: org.eclipse.equinox.jsp.jasper,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.util.tracker;version=&quot;1.3.0&quot;,javax.servlet;version=&quot;2.4&quot;,javax.servlet.http;version=&quot;2.4&quot;&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.presentations.r21' version='3.2.100.I20080512-2000'>
      <update id='org.eclipse.ui.presentations.r21' range='[0.0.0,3.2.100.I20080512-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='R21 Presentation Plug-in'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.presentations.r21' version='3.2.100.I20080512-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.presentations.r21' version='3.2.100.I20080512-2000'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations.r21' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations.r21.widgets' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='3'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.presentations.r21' version='3.2.100.I20080512-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 3.2.100.I20080512-2000&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-Activator: org.eclipse.ui.internal.presentations.r21.R21PresentationPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.ui.internal.presentations;x-internal:=true,org.eclipse.ui.internal.presentations.r21;x-internal:=true,org.eclipse.ui.internal.presentations.r21.widgets;x-internal:=true&#xA;Bundle-SymbolicName: org.eclipse.ui.presentations.r21; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.jface.databinding' version='1.2.1.M20080827-0800a' singleton='false'>
      <update id='org.eclipse.jface.databinding' range='[0.0.0,1.2.1.M20080827-0800a)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JFace Data Binding for SWT and JFace'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='11'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jface.databinding' version='1.2.1.M20080827-0800a'/>
        <provided namespace='osgi.bundle' name='org.eclipse.jface.databinding' version='1.2.1.M20080827-0800a'/>
        <provided namespace='java.package' name='org.eclipse.jface.databinding.swt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.databinding.viewers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.databinding.wizard' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.databinding.provisional.swt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.databinding.provisional.viewers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.databinding.swt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.internal.databinding.viewers' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.core.databinding' range='[1.0.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.swt' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.jface.databinding' version='1.2.1.M20080827-0800a'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.jface.databinding.swt,org.eclipse.jface.databinding.viewers,org.eclipse.jface.databinding.wizard,org.eclipse.jface.internal.databinding.provisional.swt;x-internal:=true,org.eclipse.jface.internal.databinding.provisional.viewers;x-internal:=true,org.eclipse.jface.internal.databinding.swt;x-internal:=true,org.eclipse.jface.internal.databinding.viewers;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.core.databinding;bundle-version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.swt;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.jface;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.2.1.M20080827-0800a&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.jface.databinding&#xA;Import-Package: com.ibm.icu.text&#xA;Manifest-Version: 1.0&#xA;Bundle-ClassPath: .
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.console' version='3.3.0.v20080529-1300'>
      <update id='org.eclipse.ui.console' range='[0.0.0,3.3.0.v20080529-1300)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Console'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.console' version='3.3.0.v20080529-1300'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.console' version='3.3.0.v20080529-1300'/>
        <provided namespace='java.package' name='org.eclipse.ui.console' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.console.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.console' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.variables' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.console' version='3.3.0.v20080529-1300'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.ui.console.ConsolePlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui.console; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.3.0.v20080529-1300&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.console,org.eclipse.ui.console.actions,org.eclipse.ui.internal.console;x-internal:=true&#xA;Require-Bundle: org.eclipse.ui;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.ui.workbench.texteditor;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.variables;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.net' version='1.1.0.I20080604'>
      <update id='org.eclipse.core.net' range='[0.0.0,1.1.0.I20080604)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.PLUGIN_NAME' value='Internet Connection Management'/>
        <property name='df_LT.PLUGIN_PROVIDER' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%PLUGIN_NAME'/>
        <property name='org.eclipse.equinox.p2.provider' value='%PLUGIN_PROVIDER'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.net' version='1.1.0.I20080604'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.net' version='1.1.0.I20080604'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.net' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.net.proxy' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.security' range='[1.0.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='3.4.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.preferences' range='3.2.200'/>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='3.4.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='3.4.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.net' version='1.1.0.I20080604'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.core.internal.net;x-internal:=true,org.eclipse.core.net.proxy&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.security;bundle-version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.equinox.common;bundle-version=&quot;3.4.0&quot;,org.eclipse.equinox.preferences;bundle-version=&quot;3.2.200&quot;,org.eclipse.osgi;bundle-version=&quot;3.4.0&quot;,org.eclipse.equinox.registry;bundle-version=&quot;3.4.0&quot;&#xA;Bundle-Activator: org.eclipse.core.internal.net.Activator&#xA;Bundle-Name: %PLUGIN_NAME&#xA;Bundle-Version: 1.1.0.I20080604&#xA;Bundle-Vendor: %PLUGIN_PROVIDER&#xA;Bundle-SymbolicName: org.eclipse.core.net;singleton:=true&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.forms' version='3.3.101.v20080708_34x' singleton='false'>
      <update id='org.eclipse.ui.forms' range='[0.0.0,3.3.101.v20080708_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.provider-name' value='Eclipse.org'/>
        <property name='df_LT.name' value='Eclipse Forms'/>
        <property name='org.eclipse.equinox.p2.name' value='%name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%provider-name'/>
      </properties>
      <provides size='10'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.forms' version='3.3.101.v20080708_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.forms' version='3.3.101.v20080708_34x'/>
        <provided namespace='java.package' name='org.eclipse.ui.forms' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.forms.editor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.forms.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.forms.widgets' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.forms' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.forms.widgets' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='7'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.forms' version='3.3.101.v20080708_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %name&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %provider-name&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui.forms&#xA;Import-Package: com.ibm.icu.text,javax.xml.parsers,org.w3c.dom,org.xml.sax&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.3.101.v20080708_34x&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.forms,org.eclipse.ui.forms.editor,org.eclipse.ui.forms.events,org.eclipse.ui.forms.widgets,org.eclipse.ui.internal.forms;x-internal:=true,org.eclipse.ui.internal.forms.widgets;x-friends:=&quot;org.eclipse.ui.tests.forms&quot;&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.jface;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.updatesite' version='1.0.1.R34x_v20080808-1156'>
      <update id='org.eclipse.equinox.p2.updatesite' range='[0.0.0,1.0.1.R34x_v20080808-1156)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Update site repository adapter bundle (Incubation)'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.updatesite' version='1.0.1.R34x_v20080808-1156'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.updatesite' version='1.0.1.R34x_v20080808-1156'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.updatesite' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.updatesite.artifact' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.updatesite.metadata' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='27'>
        <required namespace='osgi.bundle' name='org.eclipse.ecf.filetransfer' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.ecf' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.metadata.repository' range='0.1.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.p2.artifact.repository' range='0.1.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.jobs' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.metadata.generator.features' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.generator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.security.storage' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.2.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.signedcontent' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.1.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.updatesite' version='1.0.1.R34x_v20080808-1156'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.updatesite.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.updatesite;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.p2.metadata.generator.features,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.eventbus,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.generator,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.eclipse.equinox.internal.provisional.spi.p2.artifact.repository,org.eclipse.equinox.internal.provisional.spi.p2.core.repository,org.eclipse.equinox.internal.provisional.spi.p2.metadata.repository,org.eclipse.equinox.security.storage,org.eclipse.osgi.service.resolver;version=&quot;1.2.0&quot;,org.eclipse.osgi.signedcontent;version=&quot;1.0.0&quot;,org.eclipse.osgi.util;version=&quot;1.1.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.0&quot;,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.1.R34x_v20080808-1156&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.p2.updatesite;x-internal:=true,org.eclipse.equinox.internal.p2.updatesite.artifact;x-internal:=true,org.eclipse.equinox.internal.p2.updatesite.metadata;x-internal:=true&#xA;Require-Bundle: org.eclipse.ecf.filetransfer,org.eclipse.ecf,org.eclipse.equinox.common;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.equinox.p2.metadata.repository;bundle-version=&quot;0.1.0&quot;,org.eclipse.equinox.p2.artifact.repository;bundle-version=&quot;0.1.0&quot;,org.eclipse.core.jobs;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.apache.ant' version='1.7.0.v200803061910' singleton='false'>
      <update id='org.apache.ant' range='[0.0.0,1.7.0.v200803061910)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Apache Ant'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='78'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.apache.ant' version='1.7.0.v200803061910'/>
        <provided namespace='osgi.bundle' name='org.apache.ant' version='1.7.0.v200803061910'/>
        <provided namespace='java.package' name='images' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.dispatch' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.filters' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.filters.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.helper' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.input' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.launch' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.listener' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.loader' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.compilers' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.condition' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.cvslib' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.email' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.ccm' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.clearcase' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.depend' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.depend.constantpool' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.dotnet' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.ejb' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.extension' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.extension.resolvers' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.i18n' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.image' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.j2ee' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.javacc' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.javah' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.jdepend' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.jlink' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.jsp' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.jsp.compilers' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.junit' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.junit.xsl' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.native2ascii' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.net' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.perforce' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.pvcs' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.scm' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.script' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.sos' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.sound' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.splash' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.ssh' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.starteam' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.unix' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.vss' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.optional.windows' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.taskdefs.rmic' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.conditions' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.mappers' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.optional' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.optional.depend' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.optional.image' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.resolver' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.resources' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.resources.comparators' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.resources.selectors' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.selectors' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.selectors.modifiedselector' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.types.spi' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util.depend' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util.depend.bcel' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util.facade' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util.java15' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util.optional' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.ant.util.regexp' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.bzip2' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.mail' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.tar' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.tools.zip' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.osgi' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.apache.ant' version='1.7.0.v200803061910'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.2&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %pluginName&#xA;Require-Bundle: org.eclipse.osgi&#xA;Bundle-ClassPath: lib/ant.jar,lib/ant-antlr.jar,lib/ant-apache-bcel.jar,lib/ant-apache-bsf.jar,lib/ant-apache-log4j.jar,lib/ant-apache-oro.jar,lib/ant-apache-regexp.jar,lib/ant-apache-resolver.jar,lib/ant-commons-logging.jar,lib/ant-commons-net.jar,lib/ant-jai.jar,lib/ant-javamail.jar,lib/ant-jdepend.jar,lib/ant-jmf.jar,lib/ant-jsch.jar,lib/ant-junit.jar,lib/ant-launcher.jar,lib/ant-netrexx.jar,lib/ant-nodeps.jar,lib/ant-starteam.jar,lib/ant-stylebook.jar,lib/ant-swing.jar,lib/ant-trax.jar,lib/ant-weblogic.jar&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.apache.ant&#xA;Bundle-Version: 1.7.0.v200803061910&#xA;Export-Package: images,org.apache.tools.ant,org.apache.tools.ant.dispatch,org.apache.tools.ant.filters,org.apache.tools.ant.filters.util,org.apache.tools.ant.helper,org.apache.tools.ant.input,org.apache.tools.ant.launch,org.apache.tools.ant.listener,org.apache.tools.ant.loader,org.apache.tools.ant.taskdefs,org.apache.tools.ant.taskdefs.compilers,org.apache.tools.ant.taskdefs.condition,org.apache.tools.ant.taskdefs.cvslib,org.apache.tools.ant.taskdefs.email,org.apache.tools.ant.taskdefs.optional,org.apache.tools.ant.taskdefs.optional.ccm,org.apache.tools.ant.taskdefs.optional.clearcase,org.apache.tools.ant.taskdefs.optional.depend,org.apache.tools.ant.taskdefs.optional.depend.constantpool,org.apache.tools.ant.taskdefs.optional.dotnet,org.apache.tools.ant.taskdefs.optional.ejb,org.apache.tools.ant.taskdefs.optional.extension,org.apache.tools.ant.taskdefs.optional.extension.resolvers,org.apache.tools.ant.taskdefs.optional.i18n,org.apache.tools.ant.taskdefs.optional.image,org.apache.tools.ant.taskdefs.optional.j2ee,org.apache.tools.ant.taskdefs.optional.javacc,org.apache.tools.ant.taskdefs.optional.javah,org.apache.tools.ant.taskdefs.optional.jdepend,org.apache.tools.ant.taskdefs.optional.jlink,org.apache.tools.ant.taskdefs.optional.jsp,org.apache.tools.ant.taskdefs.optional.jsp.compilers,org.apache.tools.ant.taskdefs.optional.junit,org.apache.tools.ant.taskdefs.optional.junit.xsl,org.apache.tools.ant.taskdefs.optional.native2ascii,org.apache.tools.ant.taskdefs.optional.net,org.apache.tools.ant.taskdefs.optional.perforce,org.apache.tools.ant.taskdefs.optional.pvcs,org.apache.tools.ant.taskdefs.optional.scm,org.apache.tools.ant.taskdefs.optional.script,org.apache.tools.ant.taskdefs.optional.sos,org.apache.tools.ant.taskdefs.optional.sound,org.apache.tools.ant.taskdefs.optional.splash,org.apache.tools.ant.taskdefs.optional.ssh,org.apache.tools.ant.taskdefs.optional.starteam,org.apache.tools.ant.taskdefs.optional.unix,org.apache.tools.ant.taskdefs.optional.vss,org.apache.tools.ant.taskdefs.optional.windows,org.apache.tools.ant.taskdefs.rmic,org.apache.tools.ant.types,org.apache.tools.ant.types.conditions,org.apache.tools.ant.types.mappers,org.apache.tools.ant.types.optional,org.apache.tools.ant.types.optional.depend,org.apache.tools.ant.types.optional.image,org.apache.tools.ant.types.resolver,org.apache.tools.ant.types.resources,org.apache.tools.ant.types.resources.comparators,org.apache.tools.ant.types.resources.selectors,org.apache.tools.ant.types.selectors,org.apache.tools.ant.types.selectors.modifiedselector,org.apache.tools.ant.types.spi,org.apache.tools.ant.util,org.apache.tools.ant.util.depend,org.apache.tools.ant.util.depend.bcel,org.apache.tools.ant.util.facade,org.apache.tools.ant.util.java15,org.apache.tools.ant.util.optional,org.apache.tools.ant.util.regexp,org.apache.tools.bzip2,org.apache.tools.mail,org.apache.tools.tar,org.apache.tools.zip
          </instruction>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.http.registry' version='1.0.100.v20080427-0830'>
      <update id='org.eclipse.equinox.http.registry' range='[0.0.0,1.0.100.v20080427-0830)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Http Service Registry Extensions'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.http.registry' version='1.0.100.v20080427-0830'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.http.registry' version='1.0.100.v20080427-0830'/>
        <provided namespace='java.package' name='org.eclipse.equinox.http.registry' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.registry' range='0.0.0'/>
        <required namespace='java.package' name='javax.servlet' range='2.3.0'/>
        <required namespace='java.package' name='javax.servlet.http' range='2.3.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.http' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.1'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.http.registry' version='1.0.100.v20080427-0830'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.http.registry;version=&quot;1.0.0&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common,org.eclipse.equinox.registry&#xA;Bundle-Activator: org.eclipse.equinox.http.registry.internal.Activator&#xA;Bundle-Name: %bundleName&#xA;Bundle-Version: 1.0.100.v20080427-0830&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.http.registry;singleton:=true&#xA;Import-Package: javax.servlet;version=&quot;2.3&quot;,javax.servlet.http;version=&quot;2.3&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.http;version=&quot;1.2.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.util.tracker;version=&quot;1.3.1&quot;&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.platform' version='3.3.101.v200809111700'>
      <update id='org.eclipse.platform' range='[0.0.0,3.3.101.v200809111700)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Eclipse Platform'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='5'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform' version='3.3.101.v200809111700'/>
        <provided namespace='osgi.bundle' name='org.eclipse.platform' version='3.3.101.v200809111700'/>
        <provided namespace='java.package' name='org.eclipse.platform.internal' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.ui.intro' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.cheatsheets' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.ui' range='[3.1.0,4.0.0)' optional='true'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.intro.universal' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.ide.application' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.platform' version='3.3.101.v200809111700'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.platform.internal;x-internal:=true&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.ui.intro;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.cheatsheets;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.ui.forms;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;;resolution:=optional,org.eclipse.update.ui;bundle-version=&quot;[3.1.0,4.0.0)&quot;;resolution:=optional,org.eclipse.ui.intro.universal;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.ide.application;bundle-version=&quot;[1.0.0,2.0.0)&quot;&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 3.3.101.v200809111700&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.platform; singleton:=true&#xA;Import-Package: javax.xml.parsers,org.xml.sax,org.xml.sax.helpers&#xA;Manifest-Version: 1.0&#xA;Bundle-ClassPath: platform.jar
          </instruction>
          <instruction key='zipped'>
            true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='toolingorg.eclipse.platform.ide.launcher.gtk.linux.x86' version='3.4.0.M20080911-1700' singleton='false'>
      <hostRequirements size='1'>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.x86' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'/>
      </hostRequirements>
      <properties size='1'>
        <property name='org.eclipse.equinox.p2.type.fragment' value='true'/>
      </properties>
      <provides size='1'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='toolingorg.eclipse.platform.ide.launcher.gtk.linux.x86' version='3.4.0.M20080911-1700'/>
      </provides>
      <requires size='1'>
        <required namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.ide.launcher.gtk.linux.x86' range='[3.4.0.M20080911-1700,3.4.0.M20080911-1700]'/>
      </requires>
      <filter>
        (&amp; (osgi.ws=gtk) (osgi.os=linux) (osgi.arch=x86))
      </filter>
      <touchpoint id='org.eclipse.equinox.p2.native' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='2'>
          <instruction key='uninstall'>
            cleanupzip(source:@artifact, target:${installFolder});
          </instruction>
          <instruction key='install'>
            unzip(source:@artifact, target:${installFolder}); chmod(targetDir:${installFolder}, targetFile:about_files, permissions:755); chmod(targetDir:${installFolder}, targetFile:libcairo-swt.so, permissions:755); chmod(targetDir:${installFolder}, targetFile:.eclipseproduct, permissions:755); chmod(targetDir:${installFolder}, targetFile:readme, permissions:755); chmod(targetDir:${installFolder}, targetFile:eclipse, permissions:755); chmod(targetDir:${installFolder}, targetFile:notice.html, permissions:755); chmod(targetDir:${installFolder}, targetFile:configuration, permissions:755); chmod(targetDir:${installFolder}, targetFile:about.html, permissions:755); chmod(targetDir:${installFolder}, targetFile:eclipse.ini, permissions:755); chmod(targetDir:${installFolder}, targetFile:epl-v10.html, permissions:755);
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.text' version='3.4.0.v20080605-1800' singleton='false'>
      <update id='org.eclipse.text' range='[0.0.0,3.4.0.v20080605-1800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Text'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='11'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.text' version='3.4.0.v20080605-1800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.text' version='3.4.0.v20080605-1800'/>
        <provided namespace='java.package' name='org.eclipse.jface.text' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.link' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.projection' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.source' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jface.text.templates' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.text.edits' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.text.undo' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='osgi.bundle' name='org.eclipse.core.commands' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='com.ibm.icu.util' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.text' version='3.4.0.v20080605-1800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Name: %pluginName&#xA;Import-Package: com.ibm.icu.text,com.ibm.icu.util&#xA;Require-Bundle: org.eclipse.core.commands;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.equinox.common;bundle-version=&quot;[3.3.0,4.0.0)&quot;&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.text&#xA;Bundle-Version: 3.4.0.v20080605-1800&#xA;Export-Package: org.eclipse.jface.text; text=&quot;split&quot;; mandatory:=&quot;text&quot;,org.eclipse.jface.text.link; text=&quot;split&quot;; mandatory:=&quot;text&quot;,org.eclipse.jface.text.projection,org.eclipse.jface.text.source; text=&quot;split&quot;; mandatory:=&quot;text&quot;,org.eclipse.jface.text.templates; text=&quot;split&quot;; mandatory:=&quot;text&quot;,org.eclipse.text.edits,org.eclipse.text.undo
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.security.ui' version='1.0.0.v20080603-1810'>
      <update id='org.eclipse.equinox.security.ui' range='[0.0.0,1.0.0.v20080603-1810)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Security Default UI'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='11'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security.ui' version='1.0.0.v20080603-1810'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.security.ui' version='1.0.0.v20080603-1810'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.security.ui' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.ui.nls' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.ui.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.ui.storage' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.ui.storage.view' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.ui.wizard' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='14'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.security' range='[1.0.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.preferences' range='[3.2.200,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='javax.crypto.spec' range='0.0.0'/>
        <required namespace='java.package' name='javax.security.auth.x500' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.internal.provisional.service.security' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.internal.service.security' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='[1.2.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.security' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='[1.3.3,2.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.security.ui' version='1.0.0.v20080603-1810'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.equinox.internal.security.ui.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.equinox.security.ui;singleton:=true&#xA;Import-Package: javax.crypto.spec,javax.security.auth.x500,org.eclipse.osgi.internal.provisional.service.security;version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.osgi.internal.service.security,org.eclipse.osgi.service.debug;version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.osgi.service.resolver;version=&quot;[1.2.0,2.0.0)&quot;,org.eclipse.osgi.service.security;version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.osgi.util;version=&quot;[1.1.0,2.0.0)&quot;,org.osgi.framework,org.osgi.util.tracker;version=&quot;[1.3.3,2.0.0)&quot;&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 1.0.0.v20080603-1810&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.provisional.security.ui;version=&quot;1.0.0&quot;;x-friends:=&quot;org.eclipse.equinox.p2.ui&quot;,org.eclipse.equinox.internal.security.ui;x-internal:=true,org.eclipse.equinox.internal.security.ui.nls;x-internal:=true,org.eclipse.equinox.internal.security.ui.preferences;x-internal:=true,org.eclipse.equinox.internal.security.ui.storage;x-internal:=true,org.eclipse.equinox.internal.security.ui.storage.view;x-internal:=true,org.eclipse.equinox.internal.security.ui.wizard;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.security;bundle-version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.equinox.preferences;bundle-version=&quot;[3.2.200,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.core.runtime; bundle-version=&quot;[3.4.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.expressions' version='3.4.0.v20080603-2000'>
      <update id='org.eclipse.core.expressions' range='[0.0.0,3.4.0.v20080603-2000)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Expression Language'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='8'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.expressions' version='3.4.0.v20080603-2000'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.expressions' version='3.4.0.v20080603-2000'/>
        <provided namespace='java.package' name='org.eclipse.core.expressions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.expressions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.expressions.propertytester' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.expressions.util' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='2'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.expressions' version='3.4.0.v20080603-2000'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 3.4.0.v20080603-2000&#xA;Import-Package: org.w3c.dom&#xA;Bundle-Activator: org.eclipse.core.internal.expressions.ExpressionPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.3.0,4.0.0)&quot;&#xA;Export-Package: org.eclipse.core.expressions,org.eclipse.core.internal.expressions;x-internal:=true,org.eclipse.core.internal.expressions.propertytester;x-internal:=true,org.eclipse.core.internal.expressions.util;x-internal:=true&#xA;Bundle-SymbolicName: org.eclipse.core.expressions; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.platform.doc.user' version='3.4.1.r341_v20080808-0800'>
      <update id='org.eclipse.platform.doc.user' range='[0.0.0,3.4.1.r341_v20080808-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Eclipse Workbench User Guide'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='4'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.platform.doc.user' version='3.4.1.r341_v20080808-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.platform.doc.user' version='3.4.1.r341_v20080808-0800'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.platform.doc.user' version='3.4.1.r341_v20080808-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.r341_v20080808-0800&#xA;Require-Bundle: org.eclipse.help;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.platform.doc.user; singleton:=true&#xA;Bundle-Name: %pluginName&#xA;Bundle-Localization: plugin&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.navigator' version='3.3.101.M20080827-0800'>
      <update id='org.eclipse.ui.navigator' range='[0.0.0,3.3.101.M20080827-0800)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.Plugin.name' value='Common Navigator View'/>
        <property name='df_LT.Plugin.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%Plugin.name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%Plugin.providerName'/>
      </properties>
      <provides size='12'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.navigator' version='3.3.101.M20080827-0800'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.navigator' version='3.3.101.M20080827-0800'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.dnd' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.extensions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.filters' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.sorters' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.navigator.wizards' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.navigator' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.workbench' range='[3.2.1,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.1,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.2.0,4.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.navigator' version='3.3.101.M20080827-0800'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.ui.internal.navigator;x-friends:=&quot;org.eclipse.ui.navigator.ide,org.eclipse.ui.navigator.resources&quot;,org.eclipse.ui.internal.navigator.actions;x-friends:=&quot;org.eclipse.ui.navigator.ide,org.eclipse.ui.navigator.resources&quot;,org.eclipse.ui.internal.navigator.dnd;x-friends:=&quot;org.eclipse.ui.navigator.ide,org.eclipse.ui.navigator.resources&quot;,org.eclipse.ui.internal.navigator.extensions;x-friends:=&quot;org.eclipse.ui.navigator.ide,org.eclipse.ui.navigator.resources&quot;,org.eclipse.ui.internal.navigator.filters;x-friends:=&quot;org.eclipse.ui.navigator.ide,org.eclipse.ui.navigator.resources,org.eclipse.ui.tests.navigator&quot;,org.eclipse.ui.internal.navigator.sorters;x-internal:=true,org.eclipse.ui.internal.navigator.wizards;x-internal:=true,org.eclipse.ui.navigator&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.jface;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui.workbench;bundle-version=&quot;[3.2.1,4.0.0)&quot;,org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.1,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.ui.internal.navigator.NavigatorPlugin&#xA;Bundle-Name: %Plugin.name&#xA;Bundle-Version: 3.3.101.M20080827-0800&#xA;Bundle-Vendor: %Plugin.providerName&#xA;Bundle-SymbolicName: org.eclipse.ui.navigator; singleton:=true&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.security' version='1.0.1.R34x_v20080721'>
      <update id='org.eclipse.equinox.security' range='[0.0.0,1.0.1.R34x_v20080721)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Java Authentication and Authorization Service (JAAS)'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='16'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.security' version='1.0.1.R34x_v20080721'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.security' version='1.0.1.R34x_v20080721'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.auth' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.auth.events' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.auth.ext.loader' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.auth.nls' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.credentials' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.storage' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.security.storage.friends' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.security.auth' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.security.auth.credentials' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.security.auth.module' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.security.storage' version='1.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.security.storage.provider' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='18'>
        <required namespace='java.package' name='javax.crypto' range='0.0.0'/>
        <required namespace='java.package' name='javax.crypto.spec' range='0.0.0'/>
        <required namespace='java.package' name='javax.security.auth' range='0.0.0'/>
        <required namespace='java.package' name='javax.security.auth.callback' range='0.0.0'/>
        <required namespace='java.package' name='javax.security.auth.login' range='0.0.0'/>
        <required namespace='java.package' name='javax.security.auth.spi' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.internal.runtime' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.jobs' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.preferences' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='org.osgi.framework' range='[1.4.0,2.0.0)'/>
        <required namespace='java.package' name='org.osgi.service.prefs' range='[1.1.0,2.0.0)'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='[1.3.3,2.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.security' version='1.0.1.R34x_v20080721'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.security.auth;x-internal:=true,org.eclipse.equinox.internal.security.auth.events;x-internal:=true,org.eclipse.equinox.internal.security.auth.ext.loader;x-internal:=true,org.eclipse.equinox.internal.security.auth.nls;x-internal:=true,org.eclipse.equinox.internal.security.credentials;x-internal:=true,org.eclipse.equinox.internal.security.storage;x-internal:=true,org.eclipse.equinox.internal.security.storage.friends;version=&quot;1.0.0&quot;;x-friends:=&quot;org.eclipse.equinox.security.ui&quot;,org.eclipse.equinox.security.auth;version=&quot;1.0.0&quot;,org.eclipse.equinox.security.auth.credentials;version=&quot;1.0.0&quot;,org.eclipse.equinox.security.auth.module;version=&quot;1.0.0&quot;,org.eclipse.equinox.security.storage;version=&quot;1.0.0&quot;,org.eclipse.equinox.security.storage.provider;version=&quot;1.0.0&quot;&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Bundle-Activator: org.eclipse.equinox.internal.security.auth.AuthPlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.1.R34x_v20080721&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.security;singleton:=true&#xA;Import-Package: javax.crypto,javax.crypto.spec,javax.security.auth,javax.security.auth.callback,javax.security.auth.login,javax.security.auth.spi,org.eclipse.core.internal.runtime;common=split,org.eclipse.core.runtime;registry=split,org.eclipse.core.runtime.jobs,org.eclipse.core.runtime.preferences,org.eclipse.osgi.framework.log;version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.osgi.service.datalocation;version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.osgi.service.debug;version=&quot;[1.0.0,2.0.0)&quot;,org.eclipse.osgi.service.environment;version=&quot;[1.1.0,2.0.0)&quot;,org.eclipse.osgi.util;version=&quot;[1.1.0,2.0.0)&quot;,org.osgi.framework;version=&quot;[1.4.0,2.0.0)&quot;,org.osgi.service.prefs;version=&quot;[1.1.0,2.0.0)&quot;,org.osgi.util.tracker;version=&quot;[1.3.3,2.0.0)&quot;&#xA;Bundle-ActivationPolicy: lazy&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.update.ui' version='3.2.100.v20080318'>
      <update id='org.eclipse.update.ui' range='[0.0.0,3.2.100.v20080318)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Install/Update UI'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='13'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.update.ui' version='3.2.100.v20080318'/>
        <provided namespace='osgi.bundle' name='org.eclipse.update.ui' version='3.2.100.v20080318'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.parts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.properties' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.security' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.internal.ui.wizards' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.update.ui' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='8'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.core' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.update.configurator' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui.forms' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.update.ui' version='3.2.100.v20080318'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.update.internal.ui.UpdateUI&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.update.ui; singleton:=true&#xA;Import-Package: javax.xml.parsers,org.w3c.dom,org.xml.sax&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 3.2.100.v20080318&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.update.internal.ui;x-friends:=&quot;org.eclipse.update.scheduler&quot;,org.eclipse.update.internal.ui.model;x-internal:=true,org.eclipse.update.internal.ui.parts;x-internal:=true,org.eclipse.update.internal.ui.preferences;x-internal:=true,org.eclipse.update.internal.ui.properties;x-internal:=true,org.eclipse.update.internal.ui.security;x-internal:=true,org.eclipse.update.internal.ui.views;x-internal:=true,org.eclipse.update.internal.ui.wizards;x-friends:=&quot;org.eclipse.update.scheduler&quot;,org.eclipse.update.ui&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.update.core;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.update.configurator;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.ui.forms;bundle-version=&quot;[3.2.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.registry' version='3.4.0.v20080516-0950'>
      <update id='org.eclipse.equinox.registry' range='[0.0.0,3.4.0.v20080516-0950)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Extension Registry Support'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='11'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.registry' version='3.4.0.v20080516-0950'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.registry' version='3.4.0.v20080516-0950'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.adapter' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.registry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.registry.osgi' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.registry.spi' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime' version='3.4.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.dynamichelpers' version='3.4.0'/>
        <provided namespace='java.package' name='org.eclipse.core.runtime.spi' version='3.4.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='15'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.jobs' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.console' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.environment' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='0.0.0' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.storagemanager' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.registry' version='3.4.0.v20080516-0950'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Version: 3.4.0.v20080516-0950&#xA;Eclipse-ExtensibleAPI: true&#xA;Bundle-ActivationPolicy: lazy&#xA;Eclipse-LazyStart: true&#xA;Export-Package: org.eclipse.core.internal.adapter;x-internal:=true,org.eclipse.core.internal.registry;x-friends:=&quot;org.eclipse.core.runtime&quot;,org.eclipse.core.internal.registry.osgi;x-friends:=&quot;org.eclipse.core.runtime&quot;,org.eclipse.core.internal.registry.spi;x-internal:=true,org.eclipse.core.runtime;registry=split;version=&quot;3.4.0&quot;;mandatory:=registry,org.eclipse.core.runtime.dynamichelpers;version=&quot;3.4.0&quot;,org.eclipse.core.runtime.spi;version=&quot;3.4.0&quot;&#xA;Import-Package: javax.xml.parsers,org.eclipse.core.runtime.jobs;resolution:=optional,org.eclipse.osgi.framework.console;resolution:=optional,org.eclipse.osgi.service.datalocation,org.eclipse.osgi.service.debug,org.eclipse.osgi.service.environment;resolution:=optional,org.eclipse.osgi.service.resolver;resolution:=optional,org.eclipse.osgi.storagemanager,org.eclipse.osgi.util,org.osgi.framework,org.osgi.service.packageadmin,org.osgi.util.tracker,org.xml.sax,org.xml.sax.helpers&#xA;Manifest-Version: 1.0&#xA;Bundle-Activator: org.eclipse.core.internal.registry.osgi.Activator&#xA;Bundle-ClassPath: runtime_registry_compatibility.jar, .&#xA;Comment-Header: Both Eclipse-LazyStart and Bundle-ActivationPolicy are specified for compatibility with 3.2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.registry;singleton:=true&#xA;Bundle-Vendor: %providerName&#xA;Bundle-Name: %pluginName&#xA;Bundle-ManifestVersion: 2
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='javax.servlet' version='2.4.0.v200806031604' singleton='false'>
      <update id='javax.servlet' range='[0.0.0,2.4.0.v200806031604)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Servlet API Bundle'/>
        <property name='df_LT.bundleProvider' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%bundleProvider'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='javax.servlet' version='2.4.0.v200806031604'/>
        <provided namespace='osgi.bundle' name='javax.servlet' version='2.4.0.v200806031604'/>
        <provided namespace='java.package' name='javax.servlet' version='2.4.0'/>
        <provided namespace='java.package' name='javax.servlet.http' version='2.4.0'/>
        <provided namespace='java.package' name='javax.servlet.resources' version='2.4.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='javax.servlet' version='2.4.0.v200806031604'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %bundleName&#xA;Bundle-Version: 2.4.0.v200806031604&#xA;Export-Package: javax.servlet;version=&quot;2.4&quot;,javax.servlet.http;version=&quot;2.4&quot;,javax.servlet.resources;version=&quot;2.4&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-SymbolicName: javax.servlet&#xA;Bundle-Localization: plugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %bundleProvider
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.updatechecker' version='1.0.0.v20080427-2136'>
      <update id='org.eclipse.equinox.p2.updatechecker' range='[0.0.0,1.0.0.v20080427-2136)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Update Checker'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.updatechecker' version='1.0.0.v20080427-2136'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.updatechecker' version='1.0.0.v20080427-2136'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.updatechecker' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.updatechecker' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='12'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.artifact.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.director' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.engine' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.query' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.metadata.repository' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.4.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.updatechecker' version='1.0.0.v20080427-2136'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.equinox.internal.p2.updatechecker;x-internal:=true,org.eclipse.equinox.internal.provisional.p2.updatechecker;x-friends:=&quot;org.eclipse.equinox.p2.ui,org.eclipse.equinox.p2.ui.sdk,org.eclipse.equinox.p2.ui.admin&quot;&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.3.0,4.0)&quot;&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.updatechecker.Activator&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.0.0.v20080427-2136&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.updatechecker;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.p2.core.helpers,org.eclipse.equinox.internal.provisional.p2.artifact.repository,org.eclipse.equinox.internal.provisional.p2.core,org.eclipse.equinox.internal.provisional.p2.core.repository,org.eclipse.equinox.internal.provisional.p2.director,org.eclipse.equinox.internal.provisional.p2.engine,org.eclipse.equinox.internal.provisional.p2.metadata,org.eclipse.equinox.internal.provisional.p2.metadata.query,org.eclipse.equinox.internal.provisional.p2.metadata.repository,org.eclipse.equinox.internal.provisional.p2.query,org.osgi.framework;version=&quot;1.4.0&quot;&#xA;Manifest-Version: 1.0
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.apache.lucene.analysis' version='1.9.1.v20080530-1600' singleton='false'>
      <update id='org.apache.lucene.analysis' range='[0.0.0,1.9.1.v20080530-1600)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Apache Lucene Analysis'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='13'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.apache.lucene.analysis' version='1.9.1.v20080530-1600'/>
        <provided namespace='osgi.bundle' name='org.apache.lucene.analysis' version='1.9.1.v20080530-1600'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.br' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.cjk' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.cn' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.cz' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.de' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.el' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.fr' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.nl' version='0.0.0'/>
        <provided namespace='java.package' name='org.apache.lucene.analysis.ru' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='1'>
        <required namespace='osgi.bundle' name='org.apache.lucene' range='[1.9.1,2.0.0)'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.apache.lucene.analysis' version='1.9.1.v20080530-1600'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.9.1.v20080530-1600&#xA;Require-Bundle: org.apache.lucene;bundle-version=&quot;[1.9.1,2.0.0)&quot;;visibility:=reexport&#xA;Export-Package: org.apache.lucene.analysis.br,org.apache.lucene.analysis.cjk,org.apache.lucene.analysis.cn,org.apache.lucene.analysis.cz,org.apache.lucene.analysis.de,org.apache.lucene.analysis.el,org.apache.lucene.analysis.fr,org.apache.lucene.analysis.nl,org.apache.lucene.analysis.ru&#xA;Bundle-SymbolicName: org.apache.lucene.analysis&#xA;Bundle-Localization: plugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.workbench' version='3.4.1.M20080827-0800a'>
      <update id='org.eclipse.ui.workbench' range='[0.0.0,3.4.1.M20080827-0800a)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Workbench'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='80'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.workbench' version='3.4.1.M20080827-0800a'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.workbench' version='3.4.1.M20080827-0800a'/>
        <provided namespace='java.package' name='org.eclipse.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.about' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.activities' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.application' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.branding' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.contexts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.dnd' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.fieldassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.handlers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.help' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.about' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.actions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.activities' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.activities.ws' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.application' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.browser' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.commands' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.contexts' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.decorators' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.dialogs' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.dnd' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.editorsupport' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.expressions' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.handlers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.help' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.intro' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.keys' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.keys.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.layout' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.menus' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.misc' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.part' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations.classic' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations.defaultpresentation' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.presentations.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.progress' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.provisional.application' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.provisional.presentations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.quickaccess' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.registry' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.services' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.splash' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.statushandlers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.testing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.themes' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.tweaklets' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.wizards' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.wizards.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.intro' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.keys' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.menus' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.model' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.operations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.part' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.plugin' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.preferences' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.presentations' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.progress' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.services' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.splash' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.statushandlers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.swt' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.testing' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.themes' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.views' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.wizards' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='11'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.help' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.swt' range='[3.3.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.databinding' range='[1.1.0,2.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.databinding' range='[1.0.0,2.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.workbench' version='3.4.1.M20080827-0800a'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: compatibility.jar,.&#xA;Bundle-Version: 3.4.1.M20080827-0800a&#xA;Import-Package: com.ibm.icu.text,javax.xml.parsers,org.w3c.dom,org.xml.sax&#xA;Bundle-Activator: org.eclipse.ui.internal.WorkbenchPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.help;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.jface;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.swt;bundle-version=&quot;[3.3.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.jface.databinding;bundle-version=&quot;[1.1.0,2.0.0)&quot;,org.eclipse.core.databinding;bundle-version=&quot;[1.0.0,2.0.0)&quot;&#xA;Export-Package: org.eclipse.ui;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;,org.eclipse.ui.about,org.eclipse.ui.actions;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;,org.eclipse.ui.activities,org.eclipse.ui.application,org.eclipse.ui.branding,org.eclipse.ui.browser;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;,org.eclipse.ui.commands,org.eclipse.ui.contexts,org.eclipse.ui.dialogs;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;,org.eclipse.ui.dnd,org.eclipse.ui.fieldassist,org.eclipse.ui.handlers,org.eclipse.ui.help,org.eclipse.ui.internal;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;; x-friends:=&quot;org.eclipse.ui,  org.eclipse.ui.intro,  org.eclipse.ui.ide,  org.eclipse.ui.presentations.r21&quot;,org.eclipse.ui.internal.about;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.actions;x-internal:=true,org.eclipse.ui.internal.activities;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.activities.ws;x-internal:=true,org.eclipse.ui.internal.application;x-internal:=true,org.eclipse.ui.internal.browser;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;;x-internal:=true,org.eclipse.ui.internal.commands;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.contexts;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.decorators;x-friends:=&quot;org.eclipse.ui.ide&quot;,org.eclipse.ui.internal.dialogs;x-internal:=true,org.eclipse.ui.internal.dnd;x-friends:=&quot;org.eclipse.ui.intro,org.eclipse.ui.presentations.r21&quot;,org.eclipse.ui.internal.editorsupport;x-internal:=true,org.eclipse.ui.internal.expressions;x-internal:=true,org.eclipse.ui.internal.handlers;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.help;x-internal:=true,org.eclipse.ui.internal.intro;x-internal:=true,org.eclipse.ui.internal.keys;x-internal:=true,org.eclipse.ui.internal.keys.model;x-internal:=true,org.eclipse.ui.internal.layout;x-friends:=&quot;org.eclipse.ui.presentations.r21,org.eclipse.ui.intro&quot;,org.eclipse.ui.internal.menus;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.misc;x-internal:=true,org.eclipse.ui.internal.model;x-internal:=true,org.eclipse.ui.internal.operations;x-internal:=true,org.eclipse.ui.internal.part;x-internal:=true,org.eclipse.ui.internal.preferences;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.presentations;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;;x-friends:=&quot;org.eclipse.ui.presentations.r21&quot;,org.eclipse.ui.internal.presentations.classic;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.presentations.defaultpresentation;x-internal:=true,org.eclipse.ui.internal.presentations.util;x-friends:=&quot;org.eclipse.ui.presentations.r21&quot;,org.eclipse.ui.internal.progress;x-internal:=true,org.eclipse.ui.internal.provisional.application;x-internal:=true,org.eclipse.ui.internal.provisional.presentations;x-internal:=true,org.eclipse.ui.internal.quickaccess;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.registry;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.services;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.splash;x-internal:=true,org.eclipse.ui.internal.statushandlers;x-internal:=true,org.eclipse.ui.internal.testing;x-internal:=true,org.eclipse.ui.internal.themes;x-friends:=&quot;org.eclipse.ui&quot;,org.eclipse.ui.internal.tweaklets;x-internal:=true,org.eclipse.ui.internal.util;x-friends:=&quot;org.eclipse.ui,org.eclipse.ui.presentations.r21,org.eclipse.ui.ide&quot;,org.eclipse.ui.internal.wizards;x-internal:=true,org.eclipse.ui.internal.wizards.preferences;x-internal:=true,org.eclipse.ui.intro,org.eclipse.ui.keys,org.eclipse.ui.menus,org.eclipse.ui.model;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;,org.eclipse.ui.operations,org.eclipse.ui.part;ui.workbench=split;mandatory:=&quot;ui.workbench&quot;,org.eclipse.ui.plugin,org.eclipse.ui.preferences,org.eclipse.ui.presentations,org.eclipse.ui.progress,org.eclipse.ui.services,org.eclipse.ui.splash,org.eclipse.ui.statushandlers,org.eclipse.ui.swt,org.eclipse.ui.testing,org.eclipse.ui.themes,org.eclipse.ui.views,org.eclipse.ui.wizards&#xA;Bundle-SymbolicName: org.eclipse.ui.workbench; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.core.databinding' version='1.1.1.M20080827-0800b' singleton='false'>
      <update id='org.eclipse.core.databinding' range='[0.0.0,1.1.1.M20080827-0800b)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JFace Data Binding'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='20'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.core.databinding' version='1.1.1.M20080827-0800b'/>
        <provided namespace='osgi.bundle' name='org.eclipse.core.databinding' version='1.1.1.M20080827-0800b'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.conversion' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.observable' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.observable.list' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.observable.map' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.observable.masterdetail' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.observable.set' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.observable.value' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.util' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.databinding.validation' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding.conversion' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding.observable' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding.observable.masterdetail' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding.observable.tree' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.core.internal.databinding.validation' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='[3.2.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='[1.4.0,2.0.0)' optional='true'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='[1.3.3,2.0.0)' optional='true'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='[1.0.0,2.0.0)' optional='true'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.core.databinding' version='1.1.1.M20080827-0800b'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: CDC-1.1/Foundation-1.1,J2SE-1.4&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;[3.2.0,4.0.0)&quot;&#xA;Bundle-Activator: org.eclipse.core.internal.databinding.Activator&#xA;Bundle-Vendor: %providerName&#xA;Import-Package-Comment: see http://wiki.eclipse.org/&#xA;Manifest-Version: 1.0&#xA;Bundle-Version: 1.1.1.M20080827-0800b&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.core.databinding,org.eclipse.core.databinding.conversion;x-internal:=false,org.eclipse.core.databinding.observable,org.eclipse.core.databinding.observable.list;x-internal:=false,org.eclipse.core.databinding.observable.map,org.eclipse.core.databinding.observable.masterdetail,org.eclipse.core.databinding.observable.set;x-internal:=false,org.eclipse.core.databinding.observable.value;x-internal:=false,org.eclipse.core.databinding.util,org.eclipse.core.databinding.validation;x-internal:=false,org.eclipse.core.internal.databinding;x-friends:=&quot;org.eclipse.core.databinding.beans&quot;,org.eclipse.core.internal.databinding.conversion;x-friends:=&quot;org.eclipse.jface.tests.databinding&quot;,org.eclipse.core.internal.databinding.observable;x-internal:=true,org.eclipse.core.internal.databinding.observable.masterdetail;x-friends:=&quot;org.eclipse.jface.tests.databinding&quot;,org.eclipse.core.internal.databinding.observable.tree;x-friends:=&quot;org.eclipse.jface.databinding,org.eclipse.jface.tests.databinding&quot;,org.eclipse.core.internal.databinding.validation;x-friends:=&quot;org.eclipse.jface.tests.databinding&quot;&#xA;Bundle-SymbolicName: org.eclipse.core.databinding&#xA;Import-Package: com.ibm.icu.text,org.osgi.framework;version=&quot;[1.4.0,2.0.0)&quot;;resolution:=optional,org.osgi.util.tracker;version=&quot;[1.3.3,2.0.0)&quot;;resolution:=optional,org.eclipse.osgi.framework.log;version=&quot;[1.0.0,2.0.0)&quot;;resolution:=optional
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.simpleconfigurator.manipulator' version='1.0.2.R34x_v20080911'>
      <update id='org.eclipse.equinox.simpleconfigurator.manipulator' range='[0.0.0,1.0.2.R34x_v20080911)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.bundleName' value='Simple Configurator Manipulator'/>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='org.eclipse.equinox.p2.name' value='%bundleName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='6'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.simpleconfigurator.manipulator' version='1.0.2.R34x_v20080911'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.simpleconfigurator.manipulator' version='1.0.2.R34x_v20080911'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.simpleconfigurator.manipulator' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.simpleconfigurator.utils' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='3.4.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.equinox' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.frameworkadmin.utils' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.configuratormanipulator' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.equinox.internal.provisional.frameworkadmin' range='0.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.service.packageadmin' range='1.2.0'/>
        <required namespace='java.package' name='org.osgi.service.startlevel' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.simpleconfigurator.manipulator' version='1.0.2.R34x_v20080911'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: CDC-1.0/Foundation-1.0,J2SE-1.4&#xA;Bundle-Name: %bundleName&#xA;Bundle-Activator: org.eclipse.equinox.internal.simpleconfigurator.manipulator.Activator&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.equinox.simpleconfigurator.manipulator;singleton:=true&#xA;Import-Package: org.eclipse.equinox.internal.frameworkadmin.equinox,org.eclipse.equinox.internal.frameworkadmin.utils,org.eclipse.equinox.internal.provisional.configuratormanipulator,org.eclipse.equinox.internal.provisional.frameworkadmin,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.service.packageadmin;version=&quot;1.2.0&quot;,org.osgi.service.startlevel;version=&quot;1.0.0&quot;,org.osgi.util.tracker&#xA;Bundle-ManifestVersion: 2&#xA;Eclipse-LazyStart: true&#xA;Bundle-Version: 1.0.2.R34x_v20080911&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.equinox.internal.simpleconfigurator.manipulator;x-friends:=&quot;org.eclipse.equinox.p2.touchpoint.eclipse&quot;,org.eclipse.equinox.internal.simpleconfigurator.utils;x-internal:=true&#xA;Require-Bundle: org.eclipse.equinox.common;bundle-version=&quot;3.4.0&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.ui.workbench.texteditor' version='3.4.1.r341_v20080827-1100'>
      <update id='org.eclipse.ui.workbench.texteditor' range='[0.0.0,3.4.1.r341_v20080827-1100)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Text Editor Framework'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='17'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.ui.workbench.texteditor' version='3.4.1.r341_v20080827-1100'/>
        <provided namespace='osgi.bundle' name='org.eclipse.ui.workbench.texteditor' version='3.4.1.r341_v20080827-1100'/>
        <provided namespace='java.package' name='org.eclipse.ui.contentassist' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor.quickdiff' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor.quickdiff.compare.equivalence' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor.quickdiff.compare.rangedifferencer' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor.rulers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.internal.texteditor.spelling' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor.link' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor.quickdiff' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor.rulers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor.spelling' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.ui.texteditor.templates' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='5'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.2.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.jface.text' range='[3.4.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='[3.4.0,4.0.0)'/>
        <required namespace='java.package' name='com.ibm.icu.text' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.ui.workbench.texteditor' version='3.4.1.r341_v20080827-1100'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Name: %pluginName&#xA;Bundle-Activator: org.eclipse.ui.internal.texteditor.TextEditorPlugin&#xA;Manifest-Version: 1.0&#xA;Bundle-Vendor: %providerName&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-SymbolicName: org.eclipse.ui.workbench.texteditor; singleton:=true&#xA;Import-Package: com.ibm.icu.text&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-Version: 3.4.1.r341_v20080827-1100&#xA;Bundle-Localization: plugin&#xA;Export-Package: org.eclipse.ui.contentassist,org.eclipse.ui.internal.texteditor;x-internal:=true; texteditor=&quot;split&quot;; mandatory:=&quot;texteditor&quot;,org.eclipse.ui.internal.texteditor.quickdiff;x-internal:=true,org.eclipse.ui.internal.texteditor.quickdiff.compare.equivalence;x-internal:=true,org.eclipse.ui.internal.texteditor.quickdiff.compare.rangedifferencer;x-internal:=true,org.eclipse.ui.internal.texteditor.rulers;x-internal:=true,org.eclipse.ui.internal.texteditor.spelling;x-internal:=true,org.eclipse.ui.texteditor; texteditor=&quot;split&quot;; mandatory:=&quot;texteditor&quot;,org.eclipse.ui.texteditor.link,org.eclipse.ui.texteditor.quickdiff,org.eclipse.ui.texteditor.rulers,org.eclipse.ui.texteditor.spelling,org.eclipse.ui.texteditor.templates&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.2.0,4.0.0)&quot;,org.eclipse.jface.text;bundle-version=&quot;[3.4.0,4.0.0)&quot;,org.eclipse.ui;bundle-version=&quot;[3.4.0,4.0.0)&quot;
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.help' version='3.3.101.v20080702_34x'>
      <update id='org.eclipse.help' range='[0.0.0,3.3.101.v20080702_34x)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.help_plugin_name' value='Help System Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%help_plugin_name'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='13'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.help' version='3.3.101.v20080702_34x'/>
        <provided namespace='osgi.bundle' name='org.eclipse.help' version='3.3.101.v20080702_34x'/>
        <provided namespace='java.package' name='org.eclipse.help' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.context' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.dynamic' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.entityresolver' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.extension' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.index' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.toc' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.help.internal.util' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='9'>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='[3.1.0,4.0.0)'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.expressions' range='[3.3.0,4.0.0)'/>
        <required namespace='java.package' name='javax.xml.parsers' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.transform' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.transform.dom' range='0.0.0'/>
        <required namespace='java.package' name='javax.xml.transform.stream' range='0.0.0'/>
        <required namespace='java.package' name='org.w3c.dom' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
        <required namespace='java.package' name='org.xml.sax.helpers' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.help' version='3.3.101.v20080702_34x'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %help_plugin_name&#xA;Bundle-Version: 3.3.101.v20080702_34x&#xA;Eclipse-LazyStart: true&#xA;Import-Package: javax.xml.parsers,javax.xml.transform,javax.xml.transform.dom,javax.xml.transform.stream,org.w3c.dom,org.xml.sax,org.xml.sax.helpers&#xA;Bundle-Activator: org.eclipse.help.internal.HelpPlugin&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.core.runtime;bundle-version=&quot;[3.1.0,4.0.0)&quot;,org.eclipse.core.expressions;bundle-version=&quot;[3.3.0,4.0.0)&quot;;visibility:=reexport&#xA;Export-Package: org.eclipse.help,org.eclipse.help.internal; x-friends:=&quot;org.eclipse.help.base,  org.eclipse.help.ui,  org.eclipse.help.webapp,  org.eclipse.ui.intro,  org.eclipse.ua.tests,  org.eclipse.ui.cheatsheets&quot;,org.eclipse.help.internal.context; x-friends:=&quot;org.eclipse.help.base,  org.eclipse.help.ui,  org.eclipse.ua.tests,  org.eclipse.help.webapp&quot;,org.eclipse.help.internal.dynamic; x-friends:=&quot;org.eclipse.ua.tests,  org.eclipse.help.ui,  org.eclipse.help.base,  org.eclipse.ui.intro,  org.eclipse.help.webapp,  org.eclipse.ui.cheatsheets&quot;,org.eclipse.help.internal.entityresolver; x-friends:=&quot;org.eclipse.help.base,  org.eclipse.help.ui,  org.eclipse.ui.cheatsheets,  org.eclipse.ua.tests&quot;,org.eclipse.help.internal.extension;x-friends:=&quot;org.eclipse.help.webapp,org.eclipse.help.base,org.eclipse.ua.tests&quot;,org.eclipse.help.internal.index; x-friends:=&quot;org.eclipse.help.webapp,  org.eclipse.ua.tests,  org.eclipse.help.ui,  org.eclipse.help.base&quot;,org.eclipse.help.internal.toc; x-friends:=&quot;org.eclipse.help.base,  org.eclipse.help.ui,  org.eclipse.help.webapp,  org.eclipse.ua.tests&quot;,org.eclipse.help.internal.util; x-friends:=&quot;org.eclipse.help.base,  org.eclipse.help.ui,  org.eclipse.help.webapp,  org.eclipse.ua.tests,  org.eclipse.ui.intro.universal&quot;&#xA;Bundle-SymbolicName: org.eclipse.help; singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ActivationPolicy: lazy&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.jsch.ui' version='1.1.100.I20080415'>
      <update id='org.eclipse.jsch.ui' range='[0.0.0,1.1.100.I20080415)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='JSch UI'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.jsch.ui' version='1.1.100.I20080415'/>
        <provided namespace='osgi.bundle' name='org.eclipse.jsch.ui' version='1.1.100.I20080415'/>
        <provided namespace='java.package' name='org.eclipse.jsch.internal.ui' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jsch.internal.ui.preference' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.jsch.ui' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='4'>
        <required namespace='osgi.bundle' name='org.eclipse.ui' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.core.runtime' range='0.0.0'/>
        <required namespace='osgi.bundle' name='org.eclipse.jsch.core' range='0.0.0'/>
        <required namespace='osgi.bundle' name='com.jcraft.jsch' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.jsch.ui' version='1.1.100.I20080415'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-ManifestVersion: 2&#xA;Export-Package: org.eclipse.jsch.internal.ui;x-internal:=true,org.eclipse.jsch.internal.ui.preference;x-internal:=true,org.eclipse.jsch.ui&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4&#xA;Bundle-Localization: plugin&#xA;Require-Bundle: org.eclipse.ui,org.eclipse.core.runtime,org.eclipse.jsch.core,com.jcraft.jsch&#xA;Bundle-Activator: org.eclipse.jsch.internal.ui.JSchUIPlugin&#xA;Bundle-Name: %pluginName&#xA;Bundle-Version: 1.1.100.I20080415&#xA;Bundle-Vendor: %providerName&#xA;Bundle-SymbolicName: org.eclipse.jsch.ui;singleton:=true&#xA;Manifest-Version: 1.0&#xA;Eclipse-LazyStart: true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.p2.core' version='1.0.0.v20080530-1237'>
      <update id='org.eclipse.equinox.p2.core' range='[0.0.0,1.0.0.v20080530-1237)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Provisioning Core'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='13'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.p2.core' version='1.0.0.v20080530-1237'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.p2.core' version='1.0.0.v20080530-1237'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.core.helpers' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.p2.persistence' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.eventbus' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.location' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.core.repository' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.p2.query' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.provisional.spi.p2.core.repository' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <requires size='12'>
        <required namespace='osgi.bundle' name='org.eclipse.equinox.common' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.core.runtime.adaptor' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.eventmgr' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.framework.log' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.internal.resolver' range='0.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.datalocation' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.debug' range='1.0.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.service.resolver' range='1.1.0'/>
        <required namespace='java.package' name='org.eclipse.osgi.util' range='1.0.0'/>
        <required namespace='java.package' name='org.osgi.framework' range='1.3.0'/>
        <required namespace='java.package' name='org.osgi.util.tracker' range='1.3.3'/>
        <required namespace='java.package' name='org.xml.sax' range='0.0.0'/>
      </requires>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.p2.core' version='1.0.0.v20080530-1237'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Bundle-Version: 1.0.0.v20080530-1237&#xA;Eclipse-LazyStart: true&#xA;Import-Package: org.eclipse.core.runtime.adaptor,org.eclipse.osgi.framework.eventmgr;version=&quot;1.0.0&quot;,org.eclipse.osgi.framework.log;version=&quot;1.0.0&quot;,org.eclipse.osgi.internal.resolver,org.eclipse.osgi.service.datalocation;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.debug;version=&quot;1.0.0&quot;,org.eclipse.osgi.service.resolver;version=&quot;1.1.0&quot;,org.eclipse.osgi.util;version=&quot;1.0.0&quot;,org.osgi.framework;version=&quot;1.3.0&quot;,org.osgi.util.tracker;version=&quot;1.3.3&quot;,org.xml.sax&#xA;Bundle-Activator: org.eclipse.equinox.internal.p2.core.Activator&#xA;Bundle-Vendor: %providerName&#xA;Require-Bundle: org.eclipse.equinox.common&#xA;Export-Package: org.eclipse.equinox.internal.p2.core;x-friends:=&quot;org.eclipse.equinox.p2.metadata.generator&quot;,org.eclipse.equinox.internal.p2.core.helpers; x-friends:=&quot;org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.director.app,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.download,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.metadata,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.frameworkadmin.test,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.repositoryoptimizer,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.updatechecker.app,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.extensionlocation&quot;,org.eclipse.equinox.internal.p2.persistence;x-friends:=&quot;org.eclipse.equinox.p2.artifact.repository,org.eclipse.equinox.p2.engine,org.eclipse.equinox.p2.metadata.repository&quot;,org.eclipse.equinox.internal.provisional.p2.core; x-friends:=&quot;org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.pde.p2.ui&quot;,org.eclipse.equinox.internal.provisional.p2.core.eventbus; x-friends:=&quot;org.eclipse.equinox.p2.metadata,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives&quot;,org.eclipse.equinox.internal.provisional.p2.core.location; x-friends:=&quot;org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.touchpoint.eclipse&quot;,org.eclipse.equinox.internal.provisional.p2.core.repository; x-friends:=&quot;org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.core,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.exemplarysetup,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.jarprocessor,  org.eclipse.equinox.p2.metadata,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.console,  org.eclipse.equinox.p2.directorywatcher&quot;,org.eclipse.equinox.internal.provisional.p2.query; x-friends:=&quot;org.eclipse.equinox.p2.artifact.optimizers,  org.eclipse.equinox.p2.artifact.processors,  org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.director,  org.eclipse.equinox.p2.director.app,  org.eclipse.equinox.p2.directorywatcher,  org.eclipse.equinox.p2.engine,  org.eclipse.equinox.p2.exemplarysetup,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.garbagecollector,  org.eclipse.equinox.p2.installer,  org.eclipse.equinox.p2.jarprocessor,  org.eclipse.equinox.p2.metadata,  org.eclipse.equinox.p2.metadata.generator,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.reconciler.dropins,  org.eclipse.equinox.p2.touchpoint.eclipse,  org.eclipse.equinox.p2.touchpoint.natives,  org.eclipse.equinox.p2.ui,  org.eclipse.equinox.p2.ui.admin,  org.eclipse.equinox.p2.ui.admin.rcp,  org.eclipse.equinox.p2.ui.sdk,  org.eclipse.equinox.p2.updatechecker,  org.eclipse.equinox.p2.updatesite,  org.eclipse.equinox.p2.console&quot;,org.eclipse.equinox.internal.provisional.spi.p2.core.repository; x-friends:=&quot;org.eclipse.equinox.p2.artifact.repository,  org.eclipse.equinox.p2.metadata.repository,  org.eclipse.equinox.p2.extensionlocation,  org.eclipse.equinox.p2.updatesite&quot;&#xA;Bundle-SymbolicName: org.eclipse.equinox.p2.core;singleton:=true&#xA;Manifest-Version: 1.0&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.1/Foundation-1.1&#xA;Bundle-Localization: plugin
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
    <unit id='org.eclipse.equinox.launcher' version='1.0.101.R34x_v20080819' singleton='false'>
      <update id='org.eclipse.equinox.launcher' range='[0.0.0,1.0.101.R34x_v20080819)' severity='0'/>
      <properties size='4'>
        <property name='df_LT.providerName' value='Eclipse.org'/>
        <property name='df_LT.pluginName' value='Equinox Launcher'/>
        <property name='org.eclipse.equinox.p2.name' value='%pluginName'/>
        <property name='org.eclipse.equinox.p2.provider' value='%providerName'/>
      </properties>
      <provides size='7'>
        <provided namespace='org.eclipse.equinox.p2.iu' name='org.eclipse.equinox.launcher' version='1.0.101.R34x_v20080819'/>
        <provided namespace='osgi.bundle' name='org.eclipse.equinox.launcher' version='1.0.101.R34x_v20080819'/>
        <provided namespace='java.package' name='org.eclipse.core.launcher' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.internal.launcher' version='0.0.0'/>
        <provided namespace='java.package' name='org.eclipse.equinox.launcher' version='0.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.eclipse.type' name='bundle' version='1.0.0'/>
        <provided namespace='org.eclipse.equinox.p2.localization' name='df_LT' version='1.0.0'/>
      </provides>
      <artifacts size='1'>
        <artifact classifier='osgi.bundle' id='org.eclipse.equinox.launcher' version='1.0.101.R34x_v20080819'/>
      </artifacts>
      <touchpoint id='org.eclipse.equinox.p2.osgi' version='1.0.0'/>
      <touchpointData size='1'>
        <instructions size='1'>
          <instruction key='manifest'>
            Bundle-Vendor: %providerName&#xA;Bundle-ManifestVersion: 2&#xA;Bundle-RequiredExecutionEnvironment: J2SE-1.4,CDC-1.0/Foundation-1.0,J2SE-1.3&#xA;Main-Class: org.eclipse.equinox.launcher.Main&#xA;Bundle-Localization: launcher&#xA;Bundle-Name: %pluginName&#xA;Bundle-ClassPath: .&#xA;Manifest-Version: 1.0&#xA;Bundle-SymbolicName: org.eclipse.equinox.launcher&#xA;Bundle-Version: 1.0.101.R34x_v20080819&#xA;Export-Package: org.eclipse.core.launcher;x-internal:=true,org.eclipse.equinox.internal.launcher;x-internal:=true,org.eclipse.equinox.launcher;x-internal:=true
          </instruction>
        </instructions>
      </touchpointData>
    </unit>
  </units>
  <iusProperties size='153'>
    <iuProperties id='org.eclipse.platform.ide.launcher.gtk.linux.x86' version='3.4.0.M20080911-1700'>
      <properties size='1'>
        <property name='unzipped|@artifact|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse' value='/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/about_files/pixman-licenses.txt|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/about_files/IJG_README|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/about_files/about_cairo.html|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/about_files/lgpl-v21.txt|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/about_files/mpl-v11.txt|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/libcairo-swt.so|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/.eclipseproduct|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/readme/readme_eclipse.html|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/eclipse|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/notice.html|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/configuration/config.ini|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/configuration/.settings/org.eclipse.equinox.p2.metadata.repository.prefs|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/configuration/.settings/org.eclipse.equinox.p2.artifact.repository.prefs|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/about.html|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/eclipse.ini|/builds/M200809111700/src/M20080911-1700/p2temp/equinox.p2.build/platform.install.linux.gtk.x86/eclipse/epl-v10.html|'/>
      </properties>
    </iuProperties>
    <iuProperties id='org.eclipse.platform.ide' version='3.4.0.M20080911-1700'>
      <properties size='2'>
        <property name='org.eclipse.equinox.p2.type.root' value='true'/>
        <property name='org.eclipse.equinox.p2.internal.inclusion.rules' value='STRICT'/>
      </properties>
    </iuProperties>
  </iusProperties>
</profile>
