       The EDEX services/capabilities are all registered/created through spring.
       By including or excluding specific spring files we can determine at startup
       which services the EDEX instance should start.
       
       If you provide no command line argument, EDEX will list available modes and exit.  
       
       All modes files are merged at startup. Modes files with modes that have
       the same name are combined so the end result is an aggregate of patterns
       in all files. For example, the ingest mode has the base mode defined
       in the ingest plug-in, but other plug-ins contribute excludes patterns.
       The modes files in these plug-ins will also have a mode named 'ingest'.
       The include and exclude tags should have regular expressions that
       are compatible with Java's Pattern class.  If you provide no <include>
       tag for a particular mode, the include defaults to .*.
       
       Each mode can define 0..n <includeMode> elements.
       The mode then becomes the sum of all <include> and <exclude> tags
       provided in all modes, including itself.  
       
       The following xml attributes can be utilized to assist in 
       structuring the correct configuration:
       
       <mode>:
            template="[true/false]", defaults to false, this is a non-bootable mode
            
       Template modes are not listed as available modes at startup. These modes are
       only useful for being included in other modes. If any mode named 'foo' has
       the template='true' attribute in any modes file, then the resulting aggregate
       'foo' mode will be a template non-bootable mode.
            
       e.g.
       <!- Ignores both foo and bar pattern files,
            includes baz and NOT bazaar files,
            the mode is bootable ->
       <mode name="sumOfIncludesAndExcludes">
          <includeMode>excludeFooFiles</includeMode>
          <includeMode>excludeBarFiles</includeMode>
          <includeMode>includeBazFiles</includeMode>
       </mode>
       <!- Template mode, not bootable ->
       <mode name="excludeFooFiles" template="true">
          <exclude>.*foo.*</exclude>
       </mode>
       <!- Template mode, not bootable ->
       <mode name="excludeBarFiles" template="true">
          <exclude>.*bar.*</exclude>
       </mode>
       <!- Template mode, not bootable,
            includes baz but NOT bazaar files ->
       <mode name="includeBazFiles" template="true">
          <include>.*baz.*</include>
          <exclude>.*bazaar.*</exclude>
       </mode>
