# datatables write addins testing

    Code
      dt_create_ui(rmd_path, row)
    Output
      <div class="gadget-container">
        <div class="gadget-title">
          <h1>Create Datatable</h1>
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
                          <p>Create a datatable in the active Rmd at the cursor from input params.</p>
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
                          <span class="help-block" align="center">
                            Rmd: 
                            /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___001_--_PN_cre.Rmd
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block" align="center">
                            cursor position (line): 
                            75
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
                            <label class="control-label" id="name-label" for="name">Datatable name:</label>
                            <input id="name" type="text" class="shiny-input-text form-control" value="samples"/>
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
                            <label class="control-label" id="ids-label" for="ids">IDs (space-separated):</label>
                            <input id="ids" type="text" class="shiny-input-text form-control" value=""/>
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
                            <div id="warningName2" class="shiny-text-output"></div>
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
                            <label class="control-label" id="data_cols-label" for="data_cols">Extra Data Cols (space-separated):</label>
                            <input id="data_cols" type="text" class="shiny-input-text form-control" value=""/>
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
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <div class="checkbox">
                              <label>
                                <input id="expand" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Expand data across IDs</span>
                              </label>
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
      </div>

---

    Code
      dt_create_template_ui(rmd_path, startrow, endrow, lt, template_datatable_name)
    Output
      <div class="gadget-container">
        <div class="gadget-title">
          <h1>Create Datatable from Template using &lt;&lt;IDS&gt;&gt; from existing datatable</h1>
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
                          <p>Create new datatable from the selected Template using &lt;&lt;IDS&gt;&gt; from existing datatable.</p>
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
                            <p align="center">
                              Rmd: 
                              /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___001_--_PN_cre.Rmd
                            </p>
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <p align="center">
                              Template - Start line: 
                              98
                            </p>
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block">
                            <p align="center">
                              End line: 
                              119
                            </p>
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
                          <h4>Select Existing Datatable:</h4>
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
                          <div class="form-group shiny-input-container">
                            <div class="checkbox">
                              <label>
                                <input id="defIDs" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Define IDs manually</span>
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
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="dt-label" for="dt">Select existing datatable to source &lt;&lt;IDS&gt;&gt; from</label>
                            <div>
                              <select id="dt" class="shiny-input-select"><option value="1" selected>samples_CNS</option></select>
                              <script type="application/json" data-for="dt" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:2;-ms-flex:2;flex:2;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:100%;">
                            <label class="control-label" id="dt_ids-label" for="dt_ids">Specify Datatable IDs manually (space separated):</label>
                            <input id="dt_ids" type="text" class="shiny-input-text form-control" value=""/>
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
                            <label class="control-label" id="template_datatable_name-label" for="template_datatable_name">Specify new Datatable name:</label>
                            <input id="template_datatable_name" type="text" class="shiny-input-text form-control" value="fix-solution-wts"/>
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
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <div class="checkbox">
                              <label>
                                <input id="allIDs" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Add All existing &amp; non-existing IDs</span>
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
                          <span style="color:red">
                            <div id="warningName2" class="shiny-text-output"></div>
                          </span>
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
      dt_create_file_ui(rmd_path, row, dtT)
    Output
      <div class="gadget-container">
        <div class="gadget-title">
          <h1>Create Datatable from File</h1>
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
                          <p>Create a datatable in the active Rmd at the cursor from a CSV (or other data) file.</p>
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
                          <span class="help-block" align="center">
                            Rmd: 
                            /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___001_--_PN_cre.Rmd
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block" align="center">
                            cursor position (line): 
                            130
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
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <label class="control-label" id="dt_name-label" for="dt_name">Datatable name:</label>
                            <input id="dt_name" type="text" class="shiny-input-text form-control" value="samples"/>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <label class="control-label" id="dtType-label" for="dtType">Select datatable type</label>
                            <div>
                              <select id="dtType" class="shiny-input-select"><option value="1" selected>CREATE</option>
      <option value="2">ADD_DATA</option>
      <option value="3">GROUP</option>
      <option value="4">RSAMPLE</option>
      <option value="5">DISPOSE</option>
      <option value="6">EXPORT</option>
      <option value="7">IMPORT</option></select>
                              <script type="application/json" data-for="dtType" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
                          <div class="form-group shiny-input-container">
                            <label class="control-label" id="file1-label" for="file1">Choose CSV File</label>
                            <div class="input-group">
                              <label class="input-group-btn input-group-prepend">
                                <span class="btn btn-default btn-file">
                                  Browse...
                                  <input id="file1" class="shiny-input-file" name="file1" type="file" style="position: absolute !important; top: -99999px !important; left: -99999px !important;" accept="text/csv,text/comma-separated-values,text/plain,.csv"/>
                                </span>
                              </label>
                              <input type="text" class="form-control" placeholder="No file selected" readonly="readonly"/>
                            </div>
                            <div id="file1_progress" class="progress active shiny-file-input-progress">
                              <div class="progress-bar"></div>
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
      </div>

---

    Code
      paste0("CREATE: ", addin_datatable_create())
    Output
      [1] "CREATE: path: /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___001_--_PN_cre.Rmd row: 129 endrow: 119 server"

---

    Code
      paste0("TEMPLATE CREATE: ", addin_datatable_create())
    Output
      [1] "TEMPLATE CREATE: path: /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___001_--_PN_cre.Rmd startrow: 98 endrow: 119 lt-names: samples_CNS template_datatable_name: fix-solution-wts server"

---

    Code
      paste0("CREATE from file: ", addin_datatable_create_from_file())
    Output
      [1] "CREATE from file: path: /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___001_--_PN_cre.Rmd row: 129 dtT names: CREATE ADD_DATA GROUP RSAMPLE DISPOSE EXPORT IMPORT server"

---

    Code
      dt_add_data_ui(rmd_path, row, lt, type, ltid)
    Output
      <div class="gadget-container">
        <div class="gadget-title">
          <h1>Add Data Datatable</h1>
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
                          <p>Add Data to a datatable in the active Rmd at the cursor position.</p>
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
                          <span class="help-block" align="center">
                            Rmd: 
                            /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___002_--_PN_ADS.Rmd
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block" align="center">
                            cursor position (line): 
                            100
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
                          <h4 align="center">Choose datatable:</h4>
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
                            <label class="control-label" id="dt-label" for="dt">Select Datatable</label>
                            <div>
                              <select id="dt" class="shiny-input-select"><option value="1" selected>samples_CNS</option></select>
                              <script type="application/json" data-for="dt" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
                          <h4 align="center">Choose datatable type: </h4>
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
                            <label class="control-label" id="type-label" for="type">Select Datatable Type</label>
                            <div>
                              <select id="type" class="shiny-input-select"><option value="1" selected>sample-first</option>
      <option value="2">variable-first</option>
      <option value="3">timetable</option></select>
                              <script type="application/json" data-for="type" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
                          <p>
                            
                            <strong>sample-first: </strong>
                            First col. is filled with all sample or group IDs, subsequent columns are data cols. specified below.
                          </p>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <p>
                            
                            <strong>variable-first: </strong>
                            First col. is filled with all data cols. specified below, subsequent columns are sample or group IDs
                          </p>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <p>
                            
                            <strong>timetable: </strong>
                            Special table syntax for planning and recording datetimes of a procedure's steps executed with different timing across sample or group IDs.
                          </p>
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
                          <h4 align="center">Choose sample/group IDs: </h4>
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
                            <label class="control-label" id="id-label" for="id">Select IDs or Groups</label>
                            <div>
                              <select id="id" class="shiny-input-select"><option value="1" selected>ALL</option>
      <option value="2">all-IDs</option>
      <option value="3">group_fix</option></select>
                              <script type="application/json" data-for="id" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
                          <p>
                            
                            <strong>ALL: </strong>
                            A single ID, ALL, is used - will add all data to all sample IDs.
                          </p>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <p>
                            
                            <strong>all-IDs: </strong>
                            All sample IDs from selected datatable are used.
                          </p>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <p>
                            
                            <strong>&lt;group-name&gt;: </strong>
                            All IDs from the selected group are used.
                          </p>
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
                          <h4 align="center">Define new Data Columns/Step Names: </h4>
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
                            <label class="control-label" id="data_cols-label" for="data_cols">Data Cols. to add (space-separated):</label>
                            <input id="data_cols" type="text" class="shiny-input-text form-control" value=""/>
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
                          <p>Define data column names as a space-separated string - these MUST be unique to this new datatable. TIMETABLE: Supply space-separated step names</p>
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
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="summarise_reps" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Summarise Sample Reps</span>
                              </label>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="all_reps" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Select ALL Sample Reps</span>
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
                          <div class="form-group shiny-input-container" style="width:95%;">
                            <label class="control-label" id="colName-label" for="colName">Timetable Column Name:</label>
                            <input id="colName" type="text" class="shiny-input-text form-control" value=""/>
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
                          <p>Define Timetable column name as a space-separated string - these MUST be unique to this new datatable.</p>
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
      dt_add_data_template_ui(rmd_path, row, lt)
    Output
      <div class="gadget-container">
        <div class="gadget-title">
          <h1>Add Data to Datatable Template from Existing Table</h1>
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
                          <p>Add IDs from existing an datatable to the datatable template in the active Rmd.</p>
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
                          <span class="help-block" align="center">
                            Rmd: 
                            /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___002_--_PN_ADS.Rmd
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block" align="center">
                            cursor position (line): 
                            196
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
                          <h4>Choose datatable:</h4>
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
                            <label class="control-label" id="dt-label" for="dt">Select Datatable</label>
                            <div>
                              <select id="dt" class="shiny-input-select"><option value="1" selected>samples_CNS</option></select>
                              <script type="application/json" data-for="dt" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="summarise_reps" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Summarise Sample Reps</span>
                              </label>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="all_reps" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Select ALL Sample Reps</span>
                              </label>
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
      </div>

---

    Code
      paste0("ADD_DATA: ", addin_datatable_add_data())
    Output
      [1] "ADD_DATA: path: /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___002_--_PN_ADS.Rmd row: 214 lt-names: samples_CNS type-names: sample-first variable-first timetable ltid-names: ALL all-IDs group_fix group_yix group_zix server"

---

    Code
      paste0("TEMPLATE ADD_DATA: ", addin_datatable_add_data())
    Output
      [1] "TEMPLATE ADD_DATA: path: /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___002_--_PN_ADS.Rmd row: 241 lt-names: samples_CNS server"

---

    Code
      dt_add_group_ui(rmd_path, row, lt, group_declaration)
    Output
      <div class="gadget-container">
        <div class="gadget-title">
          <h1>Add Groups to Datatable</h1>
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
                          <p>Add Group Data to a datatable in the active Rmd at the cursor from input params.</p>
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
                          <span class="help-block" align="center">
                            Rmd: 
                            /tmp/Rsess/_T_O_DT/0-PR-DT/PD/tn-t/tn-t___005_--_PN_ADG.Rmd
                          </span>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <span class="help-block" align="center">
                            cursor position (line): 
                            100
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
                          <p>Choose which datatable the new group data columns will be added to.</p>
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
                            <label class="control-label" id="dt-label" for="dt">Select Datatable</label>
                            <div>
                              <select id="dt" class="shiny-input-select"><option value="1" selected>samples_CNS</option></select>
                              <script type="application/json" data-for="dt" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
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
                          <p>Define the group data column names and values - these MUST be unique to the selecred datatable.</p>
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
                          <p>  Each group column name must begin with `group-`, then space-separated group names are given.</p>
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
                          <p>  Repeat for all groups.  eg. group-solvent-inc MeOH-DCM 1P group-ab-inc-conc 1mg/mL 0.5mg/mL</p>
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
                            <label class="control-label" id="data_cols-label" for="data_cols">Group Data Cols. to add (space-separated):</label>
                            <input id="data_cols" type="text" class="shiny-input-text form-control" value=""/>
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
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="summarise_reps" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Summarise Sample Reps</span>
                              </label>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div class="flexfill-item" style="position:relative;-webkit-flex:1;-ms-flex:1;flex:1;width:100%;height:100%;">
                        <div class="flexfill-item-inner" style="position:absolute;top:0;left:0;right:0;bottom:0;">
                          <div class="form-group shiny-input-container" style="width:50%;">
                            <div class="checkbox">
                              <label>
                                <input id="all_reps" type="checkbox" class="shiny-input-checkbox"/>
                                <span>Select ALL Sample Reps</span>
                              </label>
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
      </div>

