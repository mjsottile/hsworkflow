/*
	Author: Daniel Mundra
	Description: Workflow GUI blueprint file that uses wireit engine to run.
	Version: 2.0
 */

var error = ""; // Global error variable to handle errors in drawing of workflow

var workflow = {

	language: {
		languageName: "workFlow",

		/* Modules that will be used in WOOL
			Setup of a module: (Based on wireit)
				name: name of module
				container: Properties of the module
					xtype - type of module, (FormContainer - form fields, ImageContainer - Image has module)
					ddHandleClassName - class name for css
					title - title of module
					fields: form fields in the module
					terminals: ports fron which wires are drawn and connected to other modules 
		 */
		modules: [
			{
			  	"name": "comment",
			  	"container": {
				 	"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "standard",
					"title": "Comment",
					"fields": [
						{"type": "text", "inputParams": {"label": "", "name": "comment", "wirable": false, "rows":"3", "cols":"23", "id": "comment", typeInvite: "Add comments" }}
				 	]
			  	}
			},
			{
				"name": "helperfuns",
				"container": {
					"xtype": "WireIt.FormContainer",
					"ddHandleClassName": "standard",
					"title": "Helper Funs",
					"fields": [
						{"type": "text", "inputParams": {"label": "", "name": "helper", "wirable": false, "rows":"6", "cols":"23", typeInvite: "Add functions that are used in your activities" }}
					]
				}
			},
			{
				"name": "parameters",
				"container": {
					"xtype": "WireIt.FormContainer",
					"ddHandleClassName": "standard",
					"title": "Parameters",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "inargs", "wirable": false, "id": "type", typeInvite: "Input arguments" }}
					]
				}
			},
			{
				"name": "input",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "io",
					"title": "input",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
						{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						{"type": "type", "inputParams": {"label": "Input", "name": "input", "wirable": false, "id": "workfl", typeInvite: "'blank' = no input" }}
					],
					"terminals": [
						{"name": "out", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 51}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}}
					]
				}
			},
			{
				"name": "result",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "io",
					"title": "result",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
						{"type": "text", "inputParams": {"label": "Result", "name": "result", "wirable": false, "id": "workfl", typeInvite: "Main function" }}
					]
				}
			},
			{  
				"name": "workflow1",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "activity",
					"title": "workflow1",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name + #M# | #S#" }},
						{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"5", "cols":"30", "id":"workfl", typeInvite: "Use #ACT# as function name" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 44 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 102 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 72}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}

			},
			{  
				"name": "workflow2",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "activity",
					"title": "workflow2",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name + #M# | #S#" }},
						{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"5", "cols":"30", "id":"workfl", typeInvite: "Use #ACT# as function name" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 44 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 102 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output1", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 44}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1},
						{"name": "output2", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 102}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}

			},
			{  
				"name": "workflow3",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "activity",
					"title": "workflow3",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name + #M# | #S#" }},
					{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"5", "cols":"30", "id":"workfl", typeInvite: "Use #ACT# as function name" }}
				],
				"terminals": [
					{"name": "input", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 72 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
					{"name": "output1", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 44}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1},
					{"name": "output2", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 102}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
				]
				}

			},
			{  
				"name": "workflow4",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "activity",
					"title": "workflow4",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name + #M# | #S#" }},
					{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"5", "cols":"30", "id":"workfl", typeInvite: "Use #ACT# as function name" }}
				],
				"terminals": [
					{"name": "input", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 72 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
					{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 72}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
				]
				}

			},
			{
				"name": "pipe",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "pipe",
					"title": "pipe",
				"fields": [
					{"type": "type", "inputParams": {"label": "", "name": "Pipe", "wirable": false, "rows":"1", "cols":"23", "id": "pipe", typeInvite: "Name" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]} },
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]} },
				]
	         	}
			},
			{
				"name": "outpipe",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "pipe",
					"title": "outpipe",
				"fields": [
					{"type": "type", "inputParams": {"label": "", "name": "OutPipe", "wirable": false, "rows":"1", "cols":"23", "id": "outpipe", typeInvite: "Name" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pumpout","wf1out","wf2out","wf3out","wf4out","countOut","rep2out","rep3out","delout","selout","woolIOout","pipeOut"]} }
				]
	         	}
			},
			{
				"name": "take",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "take",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Take", "name": "take", "wirable": false, "id": "type", typeInvite: "Params" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 49 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 49 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},			
			{
				"name": "makeTimeout",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "makeTimeout",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Param", "name": "param", "wirable": false, "id": "type", typeInvite: "Params" }},
					{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"5", "cols":"30", "id":"workfl", typeInvite: "Timeout function" }}
				],
				"terminals": [
					{"name": "out", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 92}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}}
				]
				}
			},			
			{
				"name": "counter",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "counter",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Start", "name": "start", "wirable": false, "id": "workfl" }},
					{"type": "type", "inputParams": {"label": "End", "name": "end", "wirable": false, "id": "workfl" }}
				],
				"terminals": [
					{"name": "out", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 51}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}}
				]
				}
			},
			{
				"name": "reduce",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "reduce",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type" }},
					{"type": "type", "inputParams": {"label": "Reduce", "name": "reduce", "wirable": false, "id": "workfl", typeInvite: "String" }}
				],
				"terminals": [
					{"name": "in", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 51}, "ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}}
				]
				}
			},
			{
				"name": "replicate2",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "replicate2",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }}
				],
				"terminals": [
					{"name": "input", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 26}, "ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10},
					{"name": "output1", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 12}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}},
					{"name": "output2", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 35}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}}
				]
				}
			},
			{
				"name": "replicate3",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "replicate3",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }}
				],
				"terminals": [
					{"name": "input", "direction": [0,-1], "offsetPosition": {"right": -15, "top": 26}, "ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10},
					{"name": "output1", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 1}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}},
					{"name": "output2", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 20}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}},
					{"name": "output3", "direction": [0,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 39}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}}
				]
				}
			},
			{
				"name": "delay",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "delay",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Delay", "name": "delay", "wirable": false, "id": "type" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 49 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 49 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},
			{
				"name": "serializer",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "others",
					"title": "serializer",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},
			{
				"name": "makeDirList",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "woolio",
					"title": "WOOLIO.makeDirList",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Suffix", "name": "suffix", "wirable": false, "id": "type" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},
			{
				"name": "makeFileRead",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "woolio",
					"title": "WOOLIO.makeFileRead",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},
			{
				"name": "makeFileWrite",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "woolio",
					"title": "WOOLIO.makeFileWrite",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
					{"type": "type", "inputParams": {"label": "Prefix", "name": "prefix", "wirable": false, "id": "type" }},
					{"type": "type", "inputParams": {"label": "Suffix", "name": "suffix", "wirable": false, "id": "type" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},
			{
				"name": "makeURLFetch2",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "woolio",
					"title": "WOOLIO.makeURLFetch2",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			},
			{
				"name": "makeGetTags",
				"container": {
					"xtype":"WireIt.FormContainer",
			  		"ddHandleClassName": "woolio",
					"title": "WOOLIO.makeGetTags",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }}					
				],
				"terminals": [
						{"name": "input","direction": [1,1], "offsetPosition": {"right": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 1},
						{"name": "output","direction": [-1,-1], "alwaysSrc":"true", "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}, "nMaxWires": 1 },
				]
	         	}
			}
		]
	},
	 
   /**
	* Starts the GUI
	* @method init
	* @static
	*/
   init: function() {
		this.editor = new workflow.WiringEditor(this.language);
		
		//editor.accordionView.openPanel(1);
   },
   
   /**
    * Run will go through all the wires/modules active in the gui and based on how we interpret the data
    * we will generate the haskell code and output it to a pop that will displat the code to the user.
    * Based on Dan Keith's haskell WOOL model.
    *
    * ph - placeholder
    *
    * @method run
    * @static
    */
   run: function() {
   	  // Get all wires and modules active in the gui
   	  var wires = this.editor.getValue().working.wires;
   	  var modules = this.editor.getValue().working.modules;
   	  var workflowName = this.editor.getValue().name;
   	  
	  var result = generate(wires,modules,workflowName,this.editor);
	  
	  var codeTxt = result[0].value;
	  var numCodeTxt = result[1].value;
	  var moduleFlag = result[2].value;
	  var newWorkflowName = result[3].value;
	  
	  // Pops up with the code displayed to the user.
	  // Checks if there are errors and if there are it will ouput that instead of the code.
	  // Also checks if there are modules present or not, if not it will output no workflows present.
	  // Also pass name of workflow which will be used as the filename.
	  if(modules.length != 0)	{
	    if(error == "") {
	  		showPopup(500,700,codeTxt,escapeHtml(numCodeTxt),newWorkflowName + ".hs",moduleFlag);
	  	} else {
	  		showPopup(500,700,"","--- Error Log ---\n\n" + error + "\n-----------------",newWorkflowName + ".hs",moduleFlag);
	  	}
	  } else {
	  	showPopup(500,700,"","",newWorkflowName + ".hs",moduleFlag);	  
	  }
	  
	  // Clears error log
	  error = "";	  
	},
   
   /**
    * Debug - For step by step running of WOOL code
    * @method debug
    * @static
    */
   debug: function() {
   	  // Get all wires and modules active in the gui
   	  var wires = this.editor.getValue().working.wires;
   	  var modules = this.editor.getValue().working.modules;
   	  var workflowName = this.editor.getValue().name;
   	  
	  var result = generate(wires,modules,workflowName,this.editor);
	  
	  var codeTxt = result[0].value;
	  var numCodeTxt = result[1].value;
	  var moduleFlag = result[2].value;
	  var newWorkflowName = result[3].value;
	  
	  // Pops up with the code displayed to the user.
	  // Checks if there are errors and if there are it will ouput that instead of the code.
	  // Also checks if there are modules present or not, if not it will output no workflows present.
	  // Also pass name of workflow which will be used as the filename.
	  if(modules.length != 0)	{
	    if(error == "") {
	  		showDebugPopup(500,700,codeTxt,escapeHtml(numCodeTxt),newWorkflowName + ".hs",moduleFlag);
	  	} else {
	  		showDebugPopup(500,700,"","--- Error Log ---\n\n" + error + "\n-----------------",newWorkflowName + ".hs",moduleFlag);
	  	}
	  } else {
	  	showDebugPopup(500,700,"","",newWorkflowName + ".hs",moduleFlag);	  
	  }
	  
	  // Clears error log
	  error = "";	  
	}
};

/**
 * Code Generator
 */
function generate(wires,modules,workflowName,editor) {
	  // Variables
   	  var funs = "";
   	  var pipes = "";
   	  var inpipe = "";
   	  var inArgs = "";
	  var codeTxt = "";
   	  var comment = "";
   	  var outpipe = "";
   	  var makeRed = "";
   	  var makeDel = "";
   	  var makeSel = "";
	  var inputTxt = "";
   	  var makeRepl = "";
   	  var makeCoun = "";
   	  var makeTake = "";
   	  var helpFuns = "";
   	  var pipeIndex = 0;
   	  var labelIndex = 0;
   	  var resultTxt = "";
   	  var makeTimeout = "";
   	  var makeSubFlow = "";
  	  var subflowCount = 0;
   	  var makeFunction = "";
   	  
   	  var pipeArr = new Array();
   	  
   	  // Headers required in WOOL
   	  var header =  "\n"
			   + "import WOOL \n" 
			   + "import WOOLUtil \n"
			   + "import WOOLPrimitives \n"
			   + "import qualified WOOLWeb \n"
			   + "import qualified WOOLIO \n"
			   + "import BMP \n"
			   + "import System.FilePath \n"
			   + "import Control.Monad.State \n"
			   + "import Control.Monad.Identity \n"
			   + "import Control.Concurrent \n"
			   + "import Network.HTTP; \n"
			   + "import Text.Printf \n"
			   + "import IO \n"
			   + "import List \n"
			   + "import System.IO.HVFS.Utils \n"
			   + "import Data.String.Utils \n"
			   + "import System.Path \n"
			   + "import Data.Char \n"
			   + "import System.Random; \n"
			   + "import qualified Text.HTML.TagSoup as TS \n"
			   + "import System.Directory \n\n";
	  
	  // Algorithm to find which wires to go to the same module terminal
	  var sameTerms = new Array();  	  
  	  for (var i = 0; i < wires.length; i++) {
  	  	for(var j = i+1; j < wires.length; j++) {  	  		
  	  		var srcLab1 = modules[wires[i].src.moduleId].value.label;
  	  		var tgtLab1 = modules[wires[i].tgt.moduleId].value.label;
  	  		var srcTer1 = wires[i].src.terminal;
  	  		var tgtTer1 = wires[i].tgt.terminal;
  	  		
  	  		var srcLab2 = modules[wires[j].src.moduleId].value.label;
  	  		var tgtLab2 = modules[wires[j].tgt.moduleId].value.label;
  	  		var srcTer2 = wires[j].src.terminal;
  	  		var tgtTer2 = wires[j].tgt.terminal;
  	  		
  	  		if((tgtLab1+tgtTer1) == (tgtLab2+tgtTer2))	{
  	  			sameTerms.push({wir1: i, wir2: j});
  	  		} 
  	  		
			if((srcLab1+srcTer1) == (srcLab2+srcTer2))	{
				sameTerms.push({wir1: i, wir2: j});
  	  		} 	  		
  	  	}
  	  }
  	  
  	  // For all wires creat implicit pipe names
  	  for (var i in wires) {
	    	 pipeArr.push({name: "pipe", value: "impipe"+i});
  	  }
  	  
  	  // For all the wires that go to the same module terminal replace their name with the common name
  	  for(var j in sameTerms)	{
			 pipeArr[sameTerms[j].wir1].value = pipeArr[sameTerms[j].wir2].value;
  	  }
  	    	  
  	  // Cycle through activities (modules)
  	  for (var i in modules) {
  	  	
		// Give every module a unique label by default if label not supplied
		if (modules[i].name != "pipe") {
			if(modules[i].value.label == "")	{
				modules[i].value.label = "label" + labelIndex;
				labelIndex++;
			}
  	  	}
  	  	
		// Check for subflow, if subflow exists updates th header to include the import line, and creates makeSubFlow function with workflow variable
		// and check how many input pipes and how many output pipes are there and creates the correct number of ph texts
		if(modules[i].value.label != undefined && (modules[i].value.label).indexOf("workflow") != -1)	{
			
			var sfvar = "";
			// Checks if the subflow was already imported
			if(header.indexOf((modules[i].name).toUpperCase()) == -1)	{
				sfvar = (modules[i].name).substring(0,4).toUpperCase();
				header += "import qualified " + (modules[i].name).toUpperCase() + " as " + sfvar + "SF\n\n";
			}
			
			// Setup the subflow function call
			makeSubFlow += "        makeWorkflow (" + sfvar + "SF." + modules[i].value.label + " [])";
			
			modules[i].value.label = modules[i].value.label + subflowCount;
						
			var sfpipes = editor.getPipeByName(modules[i].name);
			var sfInPipe = 0;
			var sfOutPipe = 0;
			
			// Finds how many input and output terminals are there
			for(var j = 0; j < sfpipes.modules.length; j++) {
				var m = sfpipes.modules[j];
				if( m.name == "input") {
					sfInPipe++;
				} else if(m.name == "outpipe") {
					sfOutPipe++;
				}
			}			
			
			// Depending on the number of input terminals setup the input ph
			for(var k = 0; k < sfInPipe; k++)	{
				if(k == 0) {
					makeSubFlow += "[\"" + modules[i].value.label + "inph" + k + "\"";
				} else {
					makeSubFlow += ",\"" + modules[i].value.label + "inph" + k + "\"";
				}
				
				if((k+1) == sfInPipe)	{
					makeSubFlow += "]";
					makeSubFlow += ((sfOutPipe == 0) ? " \n" : ""); 
				}
			}
			
			// Depending on the number of output terminals setup the output ph
			for(var k = 0; k < sfOutPipe; k++)	{
				if(k == 0) {
					makeSubFlow += " [\"" + modules[i].value.label + "outph" + k + "\"";
				} else {
					makeSubFlow += ",\"" + modules[i].value.label + "outph" + k + "\"";
				}
				
				if((k+1) == sfOutPipe)	{
					makeSubFlow += "] \n";
				}
			}
			subflowCount++;	
		}
  	  	  	  	
  	  	switch (modules[i].name) {
  	  		// Get all the comment boxes
  	  		case "comment": comment += "-- " + modules[i].value.comment + "\n"; break;
  	  		  	  		
  	  		// Get all the explicit pipes and makes sure that if pipe name is not supplied then it will use default unique pipe name (Need to test this more)
  	  		case "pipe": if( modules[i].value.Pipe == "")	{
							pipes += "        makePipe \"expipe" + pipeIndex + "\" \n";
							modules[i].value.Pipe = "expipe" + pipeIndex;
							pipeIndex++; 
  	  					} else {
  	  						pipes += "        makePipe \"" + modules[i].value.Pipe + "\" \n";
  	  					}
  	  					break; 
  	  		
  	  		// Get all helper functions and put it between the let .. in construct
  	  		case "helperfuns" : funs += "        " + modules[i].value.helper + "\n\n"; break;
  	  		
  	  		// Get the input arguments override, so that user can specify input arguments
  	  		case "parameters" : inArgs = modules[i].value.inargs; break;
  	  		
  	  		// Result text which has the main body to run the workflow step by step (Based on Dan Keith's model)
  	  		case "result" : resultTxt += "    " + checkTxt(modules[i].value.result,"result",modules[i].value.label); break;
  	  		
  	  		// Gets the outpipes and if the name is not supplied it will use a default unique pipe name
  	  		case "outpipe" : if( modules[i].value.OutPipe == "")	{
							pipes += "        makeOutPipe \"expipe" + pipeIndex + "\" \n";
							modules[i].value.OutPipe = "expipe" + pipeIndex;
							pipeIndex++; 
  	  					} else {
  	  						pipes += "        makeOutPipe \"" + modules[i].value.OutPipe + "\" \n";
  	  					}
  	  					break; 
  	  					
  	  		default:
  	  	}
  	  }
  	  
  	  // Cycle through pipes (wires) sources
  	  // wires[n].src.moduleId - returns the wire's source module id
  	  // modules[n] - get all the properties of the n module
  	  for (var i in wires) {  	  
		var labelTxt = modules[wires[i].src.moduleId].value.label;
		var pipeTxt = modules[wires[i].tgt.moduleId].value.Pipe;
		var outPipeTxt = modules[wires[i].tgt.moduleId].value.OutPipe;
		var typeTxt = modules[wires[i].src.moduleId].value.type;
		var funTxt = modules[wires[i].src.moduleId].value.Code;
		var mkFun = "makeFunction";
		var sflag = false;
		var mflag = false;
		var flag = true;
		
		// If pipe is not connected explicitly and it source module is not a pipe a implicit pipe is created in between src and tgt
		if(pipeTxt == undefined && modules[wires[i].src.moduleId].name != "pipe" && outPipeTxt == undefined) {
			// Checks if the pipe was already created
			if(pipes.indexOf("makePipe \"" + pipeArr[i].value + "\"") == -1 && pipes.indexOf("makeInPipe \"" + pipeArr[i].value + "\"") == -1)
				pipes += "        makePipe \"" + pipeArr[i].value + "\" \n";
				
			if(labelTxt.indexOf("push") == -1) {
				if(modules[wires[i].src.moduleId].name == "input") {
					if(pipes.indexOf("makeInPipe \"" + pipeArr[i].value + "\"") == -1) {
						pipes = pipes.replace("makePipe \"" + pipeArr[i].value + "\"","makeInPipe \"" + pipeArr[i].value + "\" phinputpipe");
					} else {
						pipes += "        makeInPipe \"" + pipeArr[i].value + "\" phinputpipe \n";
					}
				}
			}
				
			pipeTxt = pipeArr[i].value;
		}
		
		// If there is a outPipe make that the current pipeTxt
		if(outPipeTxt != undefined)	{
			pipeTxt = outPipeTxt;
		}
		
		// Checks if there are  specific keywords in the label, used to select makeFunctionS, makeFunctionM
		if(labelTxt != undefined && labelTxt.indexOf("#S#") != -1)	{
			mkFun += "S";
			labelTxt = labelTxt.replace("#S#","");
			sflag = true;
		} else if(labelTxt != undefined && labelTxt.indexOf("#M#") != -1)	{
			mkFun += "M";
			labelTxt = labelTxt.replace("#M#","");
			mflag = true;
		}
		
		// Check if the current src module is a subflow and replaces the placeholder text with the current pipe
		if(labelTxt != undefined && (labelTxt).indexOf("workflow") != -1)	{
			var terName = wires[i].src.terminal;
			var terNumb = terName.substring(terName.length-1,terName.length);
			makeSubFlow = makeSubFlow.replace(labelTxt + "outph" + terNumb,pipeTxt);						
		}
		
  	  	switch (modules[wires[i].src.moduleId].name) {
  	  	
  	  		// For input creates a push which is the initial pump primer, user can also select a blank type, by typing blank in the type field
  	  		case "input": var inputParamTxt = checkTxt(modules[wires[i].src.moduleId].value.input,"input",labelTxt);
  	  		              if(inputParamTxt != "blank") {
  	  		              	pipes = pipes.replace("phinputpipe", "[(" + ((typeTxt == "blank") ? "" : checkType(typeTxt,"input",labelTxt) + " " ) + ((typeTxt == "toSignalToken") ? "" : inputParamTxt) + ")]");  	  		
  	  		              } else {
  	  		                pipes = pipes.replace("phinputpipe", "[]");
  	  		              }
  	  		              if(labelTxt.indexOf("push") != -1) {
  	  		              	inputTxt += "        push \"" + pipeTxt + "\" (" + ((typeTxt == "blank") ? "" : checkType(typeTxt,"input",labelTxt) + " " ) + ((typeTxt == "toSignalToken") ? "" : checkTxt(modules[wires[i].src.moduleId].value.input,"input",labelTxt)) + ")\n"; 
  	  		              }
  	  		              break;
  	  		
  	  		// workflow 1: 2 -> 1, creates a makeFunction function call with ph text and the actual function definition with the code provided from the user.
  	  		// The user will use a keyword #ACT# which will be replaced by the label. This allows the user to write full functions with specific headers and also allows us
  	  		// to generate the makeFunction code.
  	  		case "workflow1": funTxt = funTxt.replace(/#ACT#/g,labelTxt);
							makeFunction += "        " + mkFun + " " + labelTxt + " " + ((sflag) ? "[]": "") + " [\"wf1ph1" + labelTxt + "\",\"wf1ph2" + labelTxt + "\"] [\"" +  pipeTxt + "\"]\n";
							funs += "        " + checkTxt(funTxt,"workflow1",labelTxt) + "\n"; break;
  	  		
  	  		// workflow 2: 2 -> 2, creates a makeFunction function call with ph text and the actual function definition with the code provided from the user
  	  		// Deals with the case of which wire was created first. We know the target pipe so we replace the ph text with the target pipe's value.
  	  		// The user will use a keyword #ACT# which will be replaced by the label. This allows the user to write full functions with specific headers and also allows us
  	  		// to generate the makeFunction code.
  	  		case "workflow2": funTxt = funTxt.replace(/#ACT#/g,labelTxt);
  	  						if (wires[i].src.terminal == "output1") {
  	  							if(makeFunction.indexOf("wf2ph3" + labelTxt) != -1) {
  	  								makeFunction = makeFunction.replace("wf2ph3" + labelTxt,pipeTxt);
  	  							} else {
  	  								makeFunction += "        " + mkFun + " " + labelTxt + " " + ((sflag) ? "[]": "") + " [\"wf2ph1" + labelTxt + "\",\"wf2ph2" + labelTxt + "\"] [\"" + pipeTxt + "\",\"wf2ph4" + labelTxt + "\"]\n";
  	  								funs += "        " + checkTxt(funTxt,"workflow2",labelTxt) + "\n";
  	  			          		}
  	  			          	} else {
  	  			          		if(makeFunction.indexOf("wf2ph4" + labelTxt) != -1) {
  	  			          			makeFunction = makeFunction.replace("wf2ph4" + labelTxt,pipeTxt);
  	  			          		} else {
  	  								makeFunction += "        " + mkFun + " " + labelTxt + " " + ((sflag) ? "[]": "") + " [\"wf2ph1" + labelTxt + "\",\"wf2ph2" + labelTxt + "\"] [\"wf2ph3" + labelTxt + "\",\"" + pipeTxt + "\"]\n";
  	  								funs += "        " + checkTxt(funTxt,"workflow2",labelTxt) + "\n";
  	  			          		}
  	  			          	}
  	  						break;
  	  		
  	  		// workflow 3: 1 -> 2, creates a makeFunction function call with ph text and the actual function definition with the code provided from the user
  	  		// Deals with the case of which wire was created first. We know the target pipe so we replace the ph text with the target pipe's value.
  	  		// The user will use a keyword #ACT# which will be replaced by the label. This allows the user to write full functions with specific headers and also allows us
  	  		// to generate the makeFunction code.  	  		
  	  		case "workflow3": funTxt = funTxt.replace(/#ACT#/g,labelTxt);
  	  						if (wires[i].src.terminal == "output1") {
  	  							if(makeFunction.indexOf("wf3ph2" + labelTxt) != -1) {
  	  								makeFunction = makeFunction.replace("wf3ph2" + labelTxt,pipeTxt);
  	  							} else {
  	  								makeFunction += "        " + mkFun + " " + labelTxt + " " + ((sflag) ? "[]": "") + " [\"wf3ph1" + labelTxt + "\"] [\"" + pipeTxt + "\",\"wf3ph3" + labelTxt + "\"]\n";  	  			          		    
  	  								funs += "        " + checkTxt(funTxt,"workflow3",labelTxt) + "\n";
  	  			          		}
  	  			          	} else {
  	  			          		if(makeFunction.indexOf("wf3ph3" + labelTxt) != -1) {
  	  			          			makeFunction = makeFunction.replace("wf3ph3" + labelTxt,pipeTxt);
  	  			          		} else {
  	  								makeFunction += "        " + mkFun + " " + labelTxt + " " + ((sflag) ? "[]": "") + " [\"wf3ph1" + labelTxt + "\"] [\"wf3ph2" + labelTxt + "\",\"" + pipeTxt + "\"]\n";
  	  			          			funs += "        " + checkTxt(funTxt,"workflow3",labelTxt) + "\n";
  	  			          		}
  	  			          	}
  	  			          	break;  						  
  	  		
  	  		// workflow 4: 1 -> 1, creates a makeFunction function call with ph text and the actual function definition with the code provided from the user.
  	  		// The user will use a keyword #ACT# which will be replaced by the label. This allows the user to write full functions with specific headers and also allows us
  	  		// to generate the makeFunction code.
  	  		case "workflow4": funTxt = funTxt.replace(/#ACT#/g,labelTxt);
  	  						makeFunction += "        " + mkFun + " " + labelTxt + " " + ((sflag) ? "[]": "") + " [\"wf4ph1" + labelTxt + "\"] [\"" +  pipeTxt + "\"]\n";
  	  						funs += "        " + checkTxt(funTxt,"workflow4",labelTxt) + "\n"; break;
  	  		
  	  		// counter: creates the counter function with start and end values and the value of the source pipe
   	  		case "counter": makeCoun += "        makeCounter " + checkTxt(modules[wires[i].src.moduleId].value.start,"counter - start",labelTxt) + " " + checkTxt(modules[wires[i].src.moduleId].value.end,"counter - end",labelTxt) + " \"" + pipeTxt + "\"\n"; break;
   	  		
   	  		// makeTimeout:
   	  		case "makeTimeout": funTxt = funTxt.replace(/#ACT#/g,labelTxt);
   	  						makeTimeout += "        makeTimeout \"" + pipeTxt + "\" " + labelTxt + " " + modules[wires[i].src.moduleId].value.param + "\n"; 
   	  						funs += "        " + checkTxt(funTxt,"makeTimeout",labelTxt) + "\n"; break;
   	  		
 	  		// replicate2: 1 -> 2, copies the pipe data to two outputs
 	  		// Deals with the case of which wire was created first. We know the target pipe so we replace the ph text with the target pipe's value 
  	  		case "replicate2": if (wires[i].src.terminal == "output1") {
  	  							if(makeRepl.indexOf("repl2ph2" + labelTxt) != -1) {
  	  								makeRepl = makeRepl.replace("repl2ph2" + labelTxt,pipeTxt);
  	  							} else {
  	  								makeRepl += "        makeReplicate \"repl2ph1" + labelTxt + "\" [\"" + pipeTxt + "\",\"repl2ph3" + labelTxt + "\"]\n";
  	  			          		}
  	  			          	} else {
  	  			          		if(makeRepl.indexOf("repl2ph3" + labelTxt) != -1) {
  	  			          			makeRepl = makeRepl.replace("repl2ph3" + labelTxt,pipeTxt);
  	  			          		} else {
  	  								makeRepl += "        makeReplicate \"repl2ph1" + labelTxt + "\" [\"repl2ph2" + labelTxt + "\",\"" + pipeTxt + "\"]\n";
  	  			          		}
  	  			          	}
  	  			          	break;
  	  		
			// replicate3: 1 -> 3, copies the pipe data to three outputs
			// Deals with the case of which wire was created first. We know the target pipe so we replace the ph text with the target pipe's value 
			case "replicate3": if (wires[i].src.terminal == "output1") {
								if(makeRepl.indexOf("repl3ph2" + labelTxt) != -1) {
									makeRepl = makeRepl.replace("repl3ph2" + labelTxt,pipeTxt);
								} else {
									makeRepl += "        makeReplicate \"repl3ph1" + labelTxt + "\" [\"" + pipeTxt + "\",\"repl3ph3" + labelTxt + "\",\"repl3ph4" + labelTxt + "\"]\n";
								}
							} else if(wires[i].src.terminal == "output2") {
								if(makeRepl.indexOf("repl3ph3" + labelTxt) != -1) {
									makeRepl = makeRepl.replace("repl3ph3" + labelTxt,pipeTxt);
								} else {
									makeRepl += "        makeReplicate \"repl3ph1" + labelTxt + "\" [\"repl3ph2\",\"" + pipeTxt + "\",\"repl3ph4" + labelTxt + "\"]\n";
								}
							} else {
								if(makeRepl.indexOf("repl3ph4" + labelTxt) != -1) {
									makeRepl = makeRepl.replace("repl3ph4" + labelTxt,pipeTxt);
								} else {
									makeRepl += "        makeReplicate \"repl3ph1" + labelTxt + "\" [\"repl3ph2" + labelTxt + "\",\"repl3ph3" + labelTxt + "\",\"" + pipeTxt + "\"]\n";
								}
							}
  	  			          	break;
  	  		
  	  		// take: 1 -> 1, pulls the specified number of outputs from a activity
  	  		case "take": makeTake += "        makeTake \"takph1" + labelTxt + "\" \"" + pipeTxt + "\" " +  checkTxt(modules[wires[i].src.moduleId].value.take,"take",labelTxt) + "\n"; break;
  	  		
  	  		// delay: 1 -> 1, delays the pipe data by the amount of ticks passed to this activity
  	  		case "delay": makeDel += "        makeDelay \"delph1" + labelTxt + "\" \"" + pipeTxt + "\" " +  checkTxt(modules[wires[i].src.moduleId].value.delay,"delay",labelTxt) + "\n"; break;
  	  		
  	  		// serializer: 1 -> 1, serializes data from one pipe to another pipe
  	  		case "serializer": makeSel += "        makeSerializer \"selph1" + labelTxt + "\" \"" + pipeTxt + "\"\n"; break;
  	  		
  	  		// makeDirList: 1 -> 1, WOOLIO component
  	  		case "makeDirList": makeSel += "        WOOLIO.makeDirListWithSuffix \"" + checkTxt(modules[wires[i].src.moduleId].value.suffix,"dirList",labelTxt) + "\" \"woolio1ph" + labelTxt + "\" \"" + pipeTxt + "\"\n"; break;
  	  		
  	  		// makeFileRead: 1 -> 1, WOOLIO component
  	  		case "makeFileRead": makeSel += "        WOOLIO.makeFileRead \"woolio2ph" + labelTxt + "\" \"" + pipeTxt + "\"\n"; break;
  	  		
  	  		// makeFileWrite: pipe -> WOOLIO component
  	  		case "makeFileWrite": var prefix = modules[wires[i].src.moduleId].value.prefix;
  	  								var suffix = modules[wires[i].src.moduleId].value.suffix;
  	  								prefix = ((prefix == "")	? "Nothing" : "(Just \"" + prefix + "\")");  	  								
  	  								suffix = ((suffix == "")	? "Nothing" : "(Just \"" + suffix + "\")");  	  								
									makeSel += "        WOOLIO.makeFileWrite " + prefix + " " + suffix + " \"woolio3ph" + labelTxt + "\" \"" + pipeTxt + "\"\n"; 
									break;
  	  		
 	  		// makeURLFetch2: 1 -> 1, WOOLIO component
  	  		case "makeURLFetch2": makeSel += "        WOOLWeb.makeURLFetch2 \"woolio4ph" + labelTxt + "\" \"" + pipeTxt + "\"\n"; break;
  	  		
  	  		// makeGetTags: 1 -> 1, WOOLIO component
  	  		case "makeGetTags": makeSel += "        WOOLWeb.makeGetTags \"woolio5ph" + labelTxt + "\" \"" + pipeTxt + "\"\n"; break;
  	  		
  	  		default:
  	  	}
  	  }
  	    	  
  	  // Cycle through pipes (wires) targets
  	  // wires[n].tgt.moduleId - returns the wire's target module id
  	  // modules[n] - get all the properties of the n module
   	  for (var i in wires) {
		var labelTxt = modules[wires[i].tgt.moduleId].value.label;
		var pipeTxt = modules[wires[i].src.moduleId].value.Pipe;
		var typeTxt = modules[wires[i].tgt.moduleId].value.type;
		
		// If pipe is not connected explicitly a implicit pipe is created in between src and tgt
		if(pipeTxt == undefined) {
			pipeTxt = pipeArr[i].value;
		}
		
		// Checks if there are  specific keywords in the label, used to select makeFunctionS, makeFunctionM
		if(labelTxt != undefined && labelTxt.indexOf("#S#") != -1)	{
			labelTxt = labelTxt.replace("#S#","");
		} else if(labelTxt != undefined && labelTxt.indexOf("#M#") != -1)	{
			labelTxt = labelTxt.replace("#M#","");
		}
		
		// Check if the current tgt module is a subflow and replaces the placeholder text with the current pipe
		if(labelTxt != undefined && (labelTxt).indexOf("workflow") != -1)	{
			var terName = wires[i].tgt.terminal;
			var terNumb = terName.substring(terName.length-1,terName.length);
			makeSubFlow = makeSubFlow.replace(labelTxt + "inph" + terNumb,pipeTxt);						
		}
  	  	
   	  	switch (modules[wires[i].tgt.moduleId].name) {   	  						
   	  		// workflow1: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "workflow1": if (wires[i].tgt.terminal == "input1") {
   	  							makeFunction = makeFunction.replace("wf1ph1" + labelTxt,pipeTxt);
   	  						} else {
   	  							makeFunction = makeFunction.replace("wf1ph2" + labelTxt,pipeTxt);
   	  						}   	  		
   	  						break;   	  		
   	  		
   	  		// workflow2: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "workflow2": if (wires[i].tgt.terminal == "input1") {
   	  							makeFunction = makeFunction.replace("wf2ph1" + labelTxt,pipeTxt);
   	  						} else {
   	  							makeFunction = makeFunction.replace("wf2ph2" + labelTxt,pipeTxt);
   	  						}   	  		
   	  						break;     	  						     	  						  
   	  		
   	  		// workflow3: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "workflow3": makeFunction = makeFunction.replace("wf3ph1" + labelTxt,pipeTxt); break;
   	  		
   	  		// workflow4: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "workflow4": makeFunction = makeFunction.replace("wf4ph1" + labelTxt,pipeTxt); break;
   	  		
   	  		// reduce: creates the reduce function with the reduce value and the value of the source pipe
   	  		case "reduce": makeRed += "        makeReduce (" + checkType(typeTxt,"reduce",labelTxt) + " " + checkTxt(modules[wires[i].tgt.moduleId].value.reduce,"reduce",labelTxt) + ") \"" + pipeTxt + "\"\n"; break;
   	  		
  	  		// replicate2: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "replicate2":  makeRepl = makeRepl.replace("repl2ph1" + labelTxt,pipeTxt); break;
   	  	
   	  		// replicate3: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "replicate3":  makeRepl = makeRepl.replace("repl3ph1" + labelTxt,pipeTxt); break;
   	  		
   	  		// take: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "take": makeTake = makeTake.replace("takph1" + labelTxt,pipeTxt); break;

   	  		// delay: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "delay": makeDel = makeDel.replace("delph1" + labelTxt,pipeTxt); break;
   	  		
   	  		// serializer: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "serializer": makeSel = makeSel.replace("selph1" + labelTxt,pipeTxt); break;
   	  		
   	  		// makeDirList: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "makeDirList": makeSel = makeSel.replace("woolio1ph" + labelTxt,pipeTxt); break;
   	  		
   	  		// makeFileRead: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "makeFileRead": makeSel = makeSel.replace("woolio2ph" + labelTxt,pipeTxt); break;
   	  		
  	  		// makeFileWrite: we know the source pipe now so we replace the ph text with source pipe's value
  	  		case "makeFileWrite": makeSel = makeSel.replace("woolio3ph" + labelTxt,pipeTxt); break;
   	  		
   	  		// makeURLFetch2: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "makeURLFetch2": makeSel = makeSel.replace("woolio4ph" + labelTxt,pipeTxt); break;
   	  		
   	  		// makeGetTags: we know the source pipe now so we replace the ph text with source pipe's value
   	  		case "makeGetTags": makeSel = makeSel.replace("woolio5ph" + labelTxt,pipeTxt); break;
   	  		   	  	
   	  		default:
   	  	}
  	  }
  	    	  
  	  // Arrange counters, replicators, delays, serializers reducers in that order
  	  makeFunction += makeCoun + makeRepl + makeDel + makeSel + makeRed + makeSubFlow;
  	  
  	  // Sets up the text with the default surroundings
  	  codeTxt += "workflow inargs =\n    let\n" + funs + "\n    in do\n" + pipes + "\n" + makeFunction + "\n" + makeTake + "\n" + makeTimeout + inputTxt + "\n        return ()";
  	  
  	  // If input arguments are supplied it will replace the default workflow inargs variable with the supplied input arguments
  	  if(inArgs != "")	{
  	  	  codeTxt = codeTxt.replace("inargs",inArgs);
  	  }
  	  
 	  // Check whether workflow is a module or not
  	  var moduleFlag = false;
  	  
  	  // If no main code is supplied it will turn the workflow into a module only
  	  if(resultTxt != "")	{  	  
		  // Create main with give result text
		  codeTxt = comment + "\n\n{-# OPTIONS_GHC -fglasgow-exts #-}" + "\nmodule " + workflowName.toUpperCase() + " where" + header + codeTxt + "\n\nmain =\n" + resultTxt;
	  } else {
	  	  codeTxt = comment + "-- This is a module\n\n{-# OPTIONS_GHC -fglasgow-exts #-}" + "\nmodule " + workflowName.toUpperCase() + " where" + header + codeTxt;
	  	  moduleFlag = true;
	  }
	  
	  // Replace workflow with workflowName+workflow. Also make the workflowName all uppercase.
	  // This is done so that workflows can be used in other workflows as subflows.
	  // workflow variable is no longer generic but is now title+workflow
	  codeTxt = codeTxt.replace("workflow",workflowName + "workflow");
	  workflowName = workflowName.toUpperCase();
	  
	  // Add lines numbers to the code for display
  	  var sp = "      ";
	  var numCodeTxt = "1" + sp + codeTxt;

	  // Get the line numbers of the code
	  var lineNums = numCodeTxt.split('\n').length;
	  numCodeTxt = numCodeTxt.replace(/\n/g,"\n\-n");
	  
	  // Put the linenumbers with correct spacing into the display code
	  for(var index = 1; index <= lineNums; index++)	{
	  	if(index >= 9)	{ sp = "     "; }
	  	if(index >= 99)	{ sp = "    "; }
		numCodeTxt = numCodeTxt.replace("\-n",index+1 + sp);
	  }
	  
	  // Check for special keywords in the code that we have banned from being used
	  codeTxt = filterCode(codeTxt);
	  
  	  var result = new Array();
  	  
  	  result.push({value: codeTxt});
  	  result.push({value: numCodeTxt});
  	  result.push({value: moduleFlag});  	  
  	  result.push({value: workflowName});
  	  
	  return result;
}

/**
 * The wiring editor is overriden to add buttons to the control bar
 */
workflow.WiringEditor = function(options) {
   workflow.WiringEditor.superclass.constructor.call(this, options);
};

YAHOO.lang.extend(workflow.WiringEditor, WireIt.WiringEditor, {
   
   /**
    * Add the buttons, run, runs, debug
    */
   renderButtons: function() {
      workflow.WiringEditor.superclass.renderButtons.call(this);
      var toolbar = YAHOO.util.Dom.get('toolbar');
      var runButton = new YAHOO.widget.Button({ label:"Generate", id:"WiringEditor-runButton", container: toolbar });
      runButton.on("click", workflow.run, workflow, true);     
      var runButton2 = new YAHOO.widget.Button({ label:"Debug", id:"WiringEditor-debugButton", container: toolbar });
      runButton2.on("click", workflow.debug, workflow, true);
   },
   
   	/**
   	 * Customize the load success handler for the composed module list
   	 */
   	onLoadSuccess: function(wirings) {
   		workflow.WiringEditor.superclass.onLoadSuccess.call(this,wirings);
   	
   		//  Customize to display composed module in the left list
   		this.updateComposedModuleList();
	},
   
   /**
    * Overwrite updateLoadPanelList to add Composed modules to the module list
    * This will be used as subflows for WOOL
    */
   updateComposedModuleList: function() { 
      
	   var l = YAHOO.util.Dom.getElementsByClassName("Subflow", "div", this.leftEl);
	   for(var i = 0 ; i < l.length ; i++) {
	   	  this.leftEl.removeChild(l[i]);
	   }
       if(YAHOO.lang.isArray(this.pipes)) {
          for(var i = 0 ; i < this.pipes.length ; i++) {
             var module = this.pipes[i];

             this.pipesByName[module.name] = module;
             
             // Add the module to the list
             var div = WireIt.cn('div', {className: "WiringEditor-module Subflow"});
             div.appendChild( WireIt.cn('span', null, null, module.name) );
             var ddProxy = new WireIt.ModuleProxy(div, this);
             ddProxy._module = {
                name: module.name,
                container: {
					"xtype": "workflow.ComposedContainer",
			  		"ddHandleClassName": "subactivity",
					"title": "workflow"
                }
             };
             this.leftEl.appendChild(div);  
          }
       }
    }
});

/**
 * ComposedContainer is a class for Container representing Pipes.
 * It automatically generates the inputEx Form from the input Params.
 * This is used for the subflows in WOOL. Depending on the number of inputs and outputs to the subflow
 * the terminals will be generated. For a subflow you will only see a label and terminals
 * @class ComposedContainer
 * @extends WireIt.inputExContainer
 * @constructor
 */
workflow.ComposedContainer = function(options, layer) {
   
   if(!options.fields) {
      
      options.fields = [{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "subfllab", "value": options.title + "workflow" }}];
      options.terminals = [];
      var terIn = 0;
      var terOut = 0;
      var inNamCnt = 0;
      var outNamCnt = 0;
      
      var pipe = workflow.editor.getPipeByName(options.title);
      
      for(var i = 0 ; i < pipe.modules.length ; i++) {
         var m = pipe.modules[i];
         if( m.name == "input") {
         	terIn++;
         }
         else if(m.name == "outpipe") {
         	terOut++;
         }
      }
      
      terIn = (terIn > 1) ? 10 : 20;
      terOut = (terOut > 1) ? 10 : 20;
      	  
      for(var i = 0 ; i < pipe.modules.length ; i++) {
         var m = pipe.modules[i];
         if( m.name == "input") {
            options.terminals.push({
               name: "input" + inNamCnt,
               "direction": [0,1], 
               "offsetPosition": {"top": terIn, "right": -15}, 
               "ddConfig": {
                   "type": "pipeIn",
                   "allowedTypes": ["pipeOut"],
                   "nMaxWires": 1
                }
            });
            terIn += 20;
            inNamCnt++;
         }
         else if(m.name == "outpipe") {
            options.terminals.push({
               name: "output" + outNamCnt,
               "direction": [0,1],
               "alwaysSrc":"true",
               "offsetPosition": {"top": terOut, "left": -15}, 
               "ddConfig": {
                   "type": "pipeOut",
                   "allowedTypes": ["pipeIn"],
                   "nMaxWires": 1
                }
            });
            terOut += 20;
            outNamCnt++;
         }
      }     
      
   }
   
   workflow.ComposedContainer.superclass.constructor.call(this, options, layer);
};

YAHOO.extend(workflow.ComposedContainer, WireIt.FormContainer, {
});

/*
 * Popup function to display code and create the form that will be used to run the php script.
 * If the code is a module it will only save the file not run it
 */
function showPopup(w,h,baseText,displayTxt,fname,moduleFlag){
	var popUp = document.getElementById("popupcontent"); 

	popUp.style.top = "5px";
	popUp.style.left = "200px";
	popUp.style.width = w + "px";
	popUp.style.height = h + "px";

	if (displayTxt == "") displayTxt = popUp.innerHTML;

	popUp.innerHTML = "<div id=\"statusbar\"><form method=\"post\" action=\"run.php\" target=\"_popup\" onsubmit=\"return openPopup()\"><button id=\"run\">" + ((moduleFlag) ? "Save":"Run") + "</button><input type=\"text\" style=\"display: none;\" name=\"fname\" value=\"" + fname + "\"><input type=\"text\" style=\"display: none;\" name=\"module\" value=\"" + ((moduleFlag) ? "true":"false") + "\"><textarea type=\"text\" style=\"display: none;\" name=\"code\">" + baseText +  "</textarea></form><button id=\"closebutton\" onclick=\"hidePopup();\"></button></div><pre>" + displayTxt + "</pre>";

	var sbar = document.getElementById("statusbar");

	sbar.style.marginLeft = "467px";

	popUp.style.visibility = "visible";
}

/*
 * Debug Popup function to display code and create the form that will be used to run the php script.
 * If the code is a module it will only save the file not run it
 */
function showDebugPopup(w,h,baseText,displayTxt,fname,moduleFlag){
	var popUp = document.getElementById("popupcontent"); 

	popUp.style.top = "5px";
	popUp.style.left = "200px";
	popUp.style.width = w + "px";
	popUp.style.height = h + "px";

	if (displayTxt == "") displayTxt = popUp.innerHTML;

	popUp.innerHTML = "<div id=\"statusbar\"><form method=\"post\" action=\"debug.php\" target=\"_popup\" onsubmit=\"return openPopup()\"><button id=\"run\">" + ((moduleFlag) ? "Save":"Debug") + "</button><input type=\"text\" style=\"display: none;\" name=\"fname\" value=\"" + fname + "\"><input type=\"text\" style=\"display: none;\" name=\"module\" value=\"" + ((moduleFlag) ? "true":"false") + "\"><textarea type=\"text\" style=\"display: none;\" name=\"code\">" + baseText +  "</textarea></form><button id=\"closebutton\" onclick=\"hidePopup();\"></button></div><pre>" + displayTxt + "</pre>";

	var sbar = document.getElementById("statusbar");

	sbar.style.marginLeft = "467px";

	popUp.style.visibility = "visible";
}

/*
 * Hide the popup
 */
function hidePopup(){
  	var popUp = document.getElementById("popupcontent");
  	
  	popUp.innerHTML = "No workflows selected";

  	popUp.style.visibility = "hidden";
}

/*
 * Check if the data passed to the gui is empty
 */
function checkTxt(str,modType,modLabel)	{
	if(str == "")	{
		error += "Empty Data Set for " + modType + " - " + modLabel + "\n";
	}
	return str;
}

/*
 * Check if the type passed to the gui is empty
 */
function checkType(str,modType,modLabel)	{
	if(str == "")	{
		error += "Empty Type for " + modType + " - " + modLabel + "\n";
	}
	return str;
}

/*
 * Check if the label exists
 */
function checkLab(labelTxt,modType)	{
	if(labelTxt == "")	{
		error += "No label is provided for " + modType + "\n";
	}
	return labelTxt;
}

/*
 * Escape html txt
 */
function escapeHtml(passTxt)	{
	if(passTxt != "")	{
		passTxt = passTxt.replace(/</g,"&lt;");
		passTxt = passTxt.replace(/>/g,"&gt;");
	}
	return passTxt;
}

/*
 * Filter code
 */
function filterCode(codeTxt)	{
    if(codeTxt.indexOf("System.system") != -1) {
    	error += "Cannot use System.system in the code";
    } else if(codeTxt.indexOf("System.CMD") != -1) {
    	error += "Cannot use System.Cmd in the code";
    } else if(codeTxt.indexOf("System.Process") != -1) {
    	error += "Cannot use System.Process in the code";
    } else if(codeTxt.indexOf("readFile") != -1) {
    	error += "Cannot use readFile in the code";
    } else if(codeTxt.indexOf("writeFile") != -1) {
    	error += "Cannot use writeFile in the code";
    } else if(codeTxt.indexOf("appendFile") != -1) {
    	error += "Cannot use appendFile in the code";
    }   
	return codeTxt;
}