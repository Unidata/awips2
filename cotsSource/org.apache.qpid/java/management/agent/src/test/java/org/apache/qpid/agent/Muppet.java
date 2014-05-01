/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.agent;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.qpid.agent.annotations.QMFObject;

@QMFObject(className = "Muppet", packageName = "org.apache.test")
public class Muppet extends Puppet
{
    private Log log = LogFactory.getLog(Muppet.class);

    public String getSomething()
    {
        return "something";
    }

    public void doSomething(String str)
    {
        log.debug(String.format("doSomething: %s", str));
    }

    public String returnSomething()
    {
        log.debug("returning something");
        return "asdf";
    }

    public Crumpet gimmieCrumpet(String asdf, int n, float f, Map foo)
    {
        log.debug(String
                .format("mmm, crumpet: %s, %s, %s, %s", asdf, n, f, foo));
        Crumpet crumpet = new Crumpet();
        crumpet.getIngredients().add("Butter");
        crumpet.getIngredients().add("Salt");
        crumpet.getIngredients().add("Flour");
        return crumpet;
    }

    public Crumpet gimmieCrumpet2()
    {
        Pikelet pik = new Pikelet();
        pik.getIngredients().add("Butter");
        pik.getIngredients().add("Salt");
        pik.getIngredients().add("Eggs");
        pik.getCrumpets().put("Crumpet1",
                this.gimmieCrumpet("2121", 1, 1, null));
        return pik;
    }

    public List gimmeLotsOfCrumpets()
    {
        log.debug("Asking for lots of Crumpets");
        ArrayList<Crumpet> returnValue = new ArrayList<Crumpet>();
        Crumpet crumpet = new Crumpet();
        crumpet.getIngredients().add("Chocolate");
        returnValue.add(crumpet);
        crumpet = new Crumpet();
        crumpet.getIngredients().add("Pecans");
        returnValue.add(crumpet);
        crumpet = new Pikelet();
        crumpet.getIngredients().add("Poached Eggs");
        returnValue.add(crumpet);
        return returnValue;
    }

    public int divideByZero()
    {
        return 1 / 0;
    }

    public Crumpet takeCrumpet(Crumpet newCrumpet)
    {
        log.debug(String.format("I gots me a crumpet: foo: '%s' bar: '%s'",
                newCrumpet.getFoo(), newCrumpet.getBar()));
        log.debug("My crumpet's class is " + newCrumpet.getClass().getName());
        for (String ingredient : newCrumpet.getIngredients())
        {
            log.debug("My crumpet is made of " + ingredient);
        }
        return newCrumpet;
    }

    public Object takeSomething(Object obj)
    {
        log.debug(String.format("I gots me a something: '%s'", obj.getClass()
                .getName()));
        return obj;
    }
}
