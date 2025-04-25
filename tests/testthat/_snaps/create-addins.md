# create addins testing

    Code
      addin_create_project_org_ui()
    Output
      <div class="gadget-container">
        <style></style>
        <div class="gadget-title">
          <h1>Create New Project Org</h1>
          <button class="btn btn-default btn-sm action-button pull-left" id="cancel" type="button">Cancel</button>
          <button class="btn btn-primary btn-sm action-button pull-right" id="done" type="button">Done</button>
        </div>
        <div class="gadget-scroll">
          <div class="gadget-content">
            <div class="gadget-absfill" style="position: absolute; top:15px;right:15px;bottom:15px;left:15px;;">
              <div class="flexfill-container flexfill-container-column" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:column;-ms-flex-direction:column;flex-direction:column;width:100%;height:100%;">
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <h5>Initialise a new Project Organisation in Selected Directory.</h5>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="organisationName-label" for="organisationName">Organisation Directory Name:</label>
                            <input id="organisationName" type="text" class="shiny-input-text form-control" value="00_ORG"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warning" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="organisationTitle-label" for="organisationTitle">Organisation Title:</label>
                            <input id="organisationTitle" type="text" class="shiny-input-text form-control" value="ORGANISATION"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warning2" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:6;-ms-flex:6;flex:6;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <pre id="dirO" class="shiny-text-output"></pre>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <button id="dir" type="button" class="shinyDirectories btn btn-default action-button" data-title="Organisation Parent Directory">Select Directory</button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

---

    Code
      paste0("CREATE ORG ADDIN: ", addin_create_project_org())
    Output
      
      projectmanagr::addin_create_project_org():
      [1] "CREATE ORG ADDIN: ui  server"

---

    Code
      addin_create_programme_ui(orgName)
    Output
      <div class="gadget-container">
        <style></style>
        <div class="gadget-title">
          <h1>Create a Programme</h1>
          <button class="btn btn-default btn-sm action-button pull-left" id="cancel" type="button">Cancel</button>
          <button class="btn btn-primary btn-sm action-button pull-right" id="done" type="button">Done</button>
        </div>
        <div class="gadget-scroll">
          <div class="gadget-content">
            <div class="gadget-absfill" style="position: absolute; top:15px;right:15px;bottom:15px;left:15px;;">
              <div class="flexfill-container flexfill-container-column" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:column;-ms-flex-direction:column;flex-direction:column;width:100%;height:100%;">
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <h3>Create a new Programme inside the Project Organisation</h3>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="orgPath-label" for="orgPath">Organisation Path:</label>
                            <input id="orgPath" type="text" class="shiny-input-text form-control" value="_T_Ots"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="programmeName-label" for="programmeName">Programme Directory Name:</label>
                            <input id="programmeName" type="text" class="shiny-input-text form-control" value="01-PROGRAMME"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningName" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="programmeTitle-label" for="programmeTitle">Programme Title:</label>
                            <input id="programmeTitle" type="text" class="shiny-input-text form-control" value="01 PROGRAMME"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

---

    Code
      paste0("CREATE PROGRAMME ADDIN: ", addin_create_programme())
    Output
      
      projectmanagr::addin_create_programme():
      [1] "CREATE PROGRAMME ADDIN: ui - orgName: _T_Ots server"

---

    Code
      addin_create_programme_section_ui(orgName)
    Output
      <div class="gadget-container">
        <style></style>
        <div class="gadget-title">
          <h1>Create a Programme Section</h1>
          <button class="btn btn-default btn-sm action-button pull-left" id="cancel" type="button">Cancel</button>
          <button class="btn btn-primary btn-sm action-button pull-right" id="done" type="button">Done</button>
        </div>
        <div class="gadget-scroll">
          <div class="gadget-content">
            <div class="gadget-absfill" style="position: absolute; top:15px;right:15px;bottom:15px;left:15px;;">
              <div class="flexfill-container flexfill-container-column" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:column;-ms-flex-direction:column;flex-direction:column;width:100%;height:100%;">
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <h3>Create a new Programme Section inside the Project Organisation</h3>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="orgPath-label" for="orgPath">Organisation Path:</label>
                            <input id="orgPath" type="text" class="shiny-input-text form-control" value="_T_Ots"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:5;-ms-flex:5;flex:5;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <pre id="dir" class="shiny-text-output"></pre>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <button id="dir" type="button" class="shinyDirectories btn btn-default action-button" data-title="Doc Parent Directory">Select Directory</button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningDirectory" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="sectionName-label" for="sectionName">Programme Section Directory Name:</label>
                            <input id="sectionName" type="text" class="shiny-input-text form-control" value="programme-section"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningName" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="sectionTitle-label" for="sectionTitle">Programme Section Title:</label>
                            <input id="sectionTitle" type="text" class="shiny-input-text form-control" value="programme section"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

---

    Code
      paste0("CREATE PROGRAMME SECTION ADDIN: ", addin_create_programme_section())
    Output
      
      projectmanagr::addin_create_programme_section():
      [1] "CREATE PROGRAMME SECTION ADDIN: ui - orgName: _T_Ots server"

---

    Code
      addin_create_project_doc_ui(orgName, get_settings_yml(orgPath))
    Output
      <div class="gadget-container">
        <style></style>
        <div class="gadget-title">
          <h1>Create a Project Document</h1>
          <button class="btn btn-default btn-sm action-button pull-left" id="cancel" type="button">Cancel</button>
          <button class="btn btn-primary btn-sm action-button pull-right" id="done" type="button">Done</button>
        </div>
        <div class="gadget-scroll">
          <div class="gadget-content">
            <div class="gadget-absfill" style="position: absolute; top:15px;right:15px;bottom:15px;left:15px;;">
              <div class="flexfill-container flexfill-container-column" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:column;-ms-flex-direction:column;flex-direction:column;width:100%;height:100%;">
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <h4>Create a Project Document inside a Programme.</h4>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="orgPath-label" for="orgPath">Organisation Path:</label>
                            <input id="orgPath" type="text" class="shiny-input-text form-control" value="_T_Ots"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:5;-ms-flex:5;flex:5;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <pre id="dir" class="shiny-text-output"></pre>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <button id="dir" type="button" class="shinyDirectories btn btn-default action-button" data-title="Doc Parent Directory">Select Directory</button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningDirectory" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:25%;">
                            <label class="control-label" id="projectPrefix-label" for="projectPrefix">Project Prefix:</label>
                            <input id="projectPrefix" type="text" class="shiny-input-text form-control" value=""/>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:75%;">
                            <label class="control-label" id="projectName-label" for="projectName">Project Name:</label>
                            <input id="projectName" type="text" class="shiny-input-text form-control" value=""/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningName" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="projectTitle-label" for="projectTitle">Project Title:</label>
                            <input id="projectTitle" type="text" class="shiny-input-text form-control" value=""/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <h3>
                            <div id="projectPathOutput" class="shiny-text-output"></div>
                          </h3>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

---

    Code
      paste0("CREATE PROJECT DOC ADDIN: ", addin_create_project_doc())
    Output
      
      projectmanagr::addin_create_project_doc():
      [1] "CREATE PROJECT DOC ADDIN: ui - orgName: _T_Ots settings-names: SiteDir FileType VolumesDir VolumesFile ConfigStatusYamlFile OrgIndexFileNamePrefix OrgProgrammeHeader OrgProgrammeFooter OrgProgrammeSummarySep ProgIndexFileNamePrefix ProgSummaryHeader ProgSummaryTitle ProgSummaryFooter ProgStrategicObjectivesHeader ProgStrategicObjectivesFooter ProgProjectsHeader ProgProjectsFooter ProgProjectSummarySep SectionIndexFileNamePrefix SectionSummaryHeader SectionSummaryTitle SectionSummaryFooter SectionProjectsHeader SectionProjectsFooter SectionProjectSummarySep ProjectPrefixSep ProjectIdentifierSep ProjectIndexSep ProjectSummaryHeader ProjectSummaryTitle ProjectSummaryFooter ProjectGoalHeader ProjectGoalTitle ProjectGoalDivider ProjectGoalSep ProjectDeliverableHeader ProjectDeliverableTitle ProjectDeliverableDivider ProjectDeliverableSep ProjectTaskHeader ProjectTaskTitle ProjectTaskDivider ProjectTaskSep ProjectTaskFooter ProjectLinkFormat ProjectTaskLogHeader ProjectTaskLogSep NoteObjectivesTodoSectionHeader NoteObjectivesHeader NoteObjectivesSep NoteObjectivesFooter NoteStorageHeader NoteStorageFooter NoteLinkFormat GroupNotePrefixSep HeaderNotePrefix HeaderNoteContentsHeader HeaderNoteContentsFooter HeaderLinkFormat SubNotePrefixSep SubNoteContentsHeader SubNoteContentsFooter SubNoteLinkFormat NoteSummaryTitle NoteGoalLinkLine NoteDeliverableLinkLine NoteTaskLinkLine ContentTitleField ContentDescriptionField ContentSourceField ContentSep ContentInsertionTitle ContentInsertionSep ContentLinkFormat ContentSummaryHeader ContentSummaryFooter ContentGraphicalAbstractHeader ContentGraphicalAbstractSvg ContentGraphicalAbstractFooter ContentBackgroundHeader ContentBackgroundFooter ContentMaterialsHeader ContentMaterialsFooter ContentEquipmentSvg ContentProcedureTemplateSvg ContentResultsLogHeader ContentResultsLogFooter ContentTroubleshootingHeader ContentTroubleshootingFooter ContentSopHeader ContentLogHeader ContentLogSep ContentFooter TodoHeader TodoHeaderTemplate TodoItem TodoItemTemplate TodoItemComplete TodoCollectionSep TodoProgrammeSep TodoProjectNoteSep TodoTaskHeader TodoGDTSep DateTimeZone DateSplit DateTimeSplit DatatableExportHeader RunUpdateOnStartup RunCompileWithUpdate FileTypeSuffix JournalDir JournalMetadataFooter JournalEventsHeader JournalEventsFooter JournalEventsInsertion JournalTodosHeader JournalTodosFooter JournalTodosInsertion GadgetWidth GadgetHeight rstudioInternalStateDir server"

---

    Code
      addin_create_prn_doc_gdt_ui(goalTitle, delTitle, taskTitle)
    Output
      <div class="gadget-container">
        <style></style>
        <div class="gadget-title">
          <h1>Add New Project Note</h1>
          <button class="btn btn-default btn-sm action-button pull-left" id="cancel" type="button">Cancel</button>
          <button class="btn btn-primary btn-sm action-button pull-right" id="done" type="button">Done</button>
        </div>
        <div class="gadget-scroll">
          <div class="gadget-content">
            <div class="gadget-absfill" style="position: absolute; top:10px;right:10px;bottom:10px;left:10px;;">
              <div class="flexfill-container flexfill-container-column" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:column;-ms-flex-direction:column;flex-direction:column;width:100%;height:100%;">
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <h5>Add a new Project Note to a Project Document.</h5>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <h3 align="center">GOAL</h3>
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <h3 align="center">DELIVERABLE</h3>
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <h3 align="center">TASK</h3>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <p align="center">EXAMPLE GOAL TITLE</p>
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <p align="center">EXAMPLE DEL TITLE</p>
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <p align="center">EXAMPLE TASK TITLE</p>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningDirectory" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:5;-ms-flex:5;flex:5;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <pre id="dir" class="shiny-text-output"></pre>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <button id="dir" type="button" class="shinyDirectories btn btn-default action-button" data-title="Note Parent Directory">Select Directory</button>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <label class="control-label" id="prefixType-label" for="prefixType">Select Project Note Type:</label>
                            <div>
                              <select id="prefixType" class="shiny-input-select"><option value="1" selected>Single</option>
      <option value="2">Group</option></select>
                              <script type="application/json" data-for="prefixType" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="addObjToHeader" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Add Objective to Header</span>
                              </label>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <br/>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span style="color:red">
                            <div id="warningName" class="shiny-text-output"></div>
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <label class="control-label" id="projectNoteName-label" for="projectNoteName">Project Note Name:</label>
                            <input id="projectNoteName" type="text" class="shiny-input-text form-control" value="Note_Name"/>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <label class="control-label" id="projectNoteTitle-label" for="projectNoteTitle">Project Note Title:</label>
                            <input id="projectNoteTitle" type="text" class="shiny-input-text form-control" value="Note Title"/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <br/>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div id="projectNotePath" class="shiny-text-output"></div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <label class="control-label" id="subNoteName-label" for="subNoteName">Project SubNote Name:</label>
                            <input id="subNoteName" type="text" class="shiny-input-text form-control" value=""/>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <label class="control-label" id="subNoteTitle-label" for="subNoteTitle">Project SubNote Title:</label>
                            <input id="subNoteTitle" type="text" class="shiny-input-text form-control" value=""/>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <br/>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                  <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                    <div class="flexfill-container flexfill-container-row" style="display:-webkit-flex;display:-ms-flexbox;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;width:100%;height:100%;">
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div id="subNotePath" class="shiny-text-output"></div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

---

    Code
      addin_open_daily_journal_ui(orgName, calDate = calDate, calendar_function = mock_calendar_input)
    Output
      <div class="gadget-container">
        <style></style>
        <script>
              $(document).on('keydown', function(e) {
                if (e.key === 'Enter') {
                  $('#done').click(); // Trigger the Done button
                }
              });
            </script>
        <div class="gadget-title">
          <h1>Open Daily Journal: Select a Day</h1>
          <button class="btn btn-default btn-sm action-button pull-left" id="cancel" type="button">Cancel</button>
          <button class="btn btn-primary btn-sm action-button pull-right" id="done" type="button">Done</button>
        </div>
        <div class="gadget-scroll">
          <div class="gadget-content">
            <div class="gadget-absfill" style="position: absolute; top:15px;right:15px;bottom:15px;left:15px;;">
              <h4 align="center">Organisation Path:  _T_Ots</h4>
              <div style="padding: 20px;" align="center">
                <div class="mock-calendar">Mock Calendar Input: calendar, Value: 2024-12-02</div>
              </div>
              <br/>
              <pre class="shiny-text-output noplaceholder" id="selected_day_text"></pre>
            </div>
          </div>
        </div>
      </div>

---

    Code
      paste0("OPEN DAILY JOURNAL ADDIN: ", addin_open_daily_journal())
    Output
      
      projectmanagr::addin_open_daily_journal():
      [1] "OPEN DAILY JOURNAL ADDIN: ui - orgName: _T_Ots server"

