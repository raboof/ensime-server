// Copyright: 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

/**
 * Please set up your development environment so that this test succeeds,
 * to prevent subtle hard-to-track-down problems.
 */
class DevelopmentEnvironmentSetupSpec extends EnsimeSpec {
  "Your development environment" should "have JAVA_HOME set" in {
    sys.env.get("JAVA_HOME").getOrElse("") !== ("")
  }
}