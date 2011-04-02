/*
	Author: Daniel Mundra
	Description: Stream Workflow GUI blueprint file that uses wireit engine to run.
	Version: 2.0
 */

var error = ""; // Global error variable to handle errors in drawing of workflow

var stream = {

	language: {
		languageName: "stream",

		/* Modules that will be used in Stream Model
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
						{"type": "text", "inputParams": {"label": "", "name": "helper", "wirable": false, "rows":"6", "cols":"23", typeInvite: "Add functions that are used in your workflows" }}
					]
				}
			},
			{
				"name": "inargs",
				"container": {
					"xtype": "WireIt.FormContainer",
					"ddHandleClassName": "standard",
					"title": "Input Args",
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
						//{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						{"type": "type", "inputParams": {"label": "Input", "name": "input", "wirable": false, "id": "general", typeInvite: "Input parameters" }}
					],
					"terminals": [
						{"name": "out", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 31}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]}}
					]
				}
			},
			{
				"name": "output",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "io",
					"title": "output",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
						{"type": "type", "inputParams": {"label": "Output", "name": "output", "wirable": false, "id": "general", typeInvite: "Opt func on output" }}
					],
					"terminals": [
						{"name": "input", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 31}, "ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}}
					]
				}
			},
			{
				"name": "main",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "io",
					"title": "main",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "label" }},
						{"type": "text", "inputParams": {"label": "Main", "name": "main", "wirable": false, "id": "general", typeInvite: "Main function" }}
					]
				}
			},
			{  
				"name": "lifter",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "lifter",
					"title": "lifter",
				"fields": [
					{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name" }},
					//{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
					{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"1", "cols":"20", "id":"lifter" }}
				],
				"terminals": [
					{"name": "input", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 33 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
					{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 33}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
				]
				}
			},
			{  
				"name": "lifter2",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "lifter",
					"title": "lifter2",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name" }},
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"1", "cols":"20", "id":"lifter" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 12 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 52 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 32}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "lifter3",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "lifter",
					"title": "lifter3",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "wflabel", typeInvite: "Function name" }},
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type3", "name": "type3", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						{"type": "text", "inputParams": {"label": "Code", "name": "Code", "wirable": false, "rows":"3", "cols":"20", "id":"lifter" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 15 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10},
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 44 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10},
						{"name": "input3", "direction": [0,-1],  "offsetPosition": {"left": -15, "top": 72}, "ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10},
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 44}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "selectLtR",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "selectLtR",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 1 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 12}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "dup",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "dup",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 12 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output1", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 1}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1},
						{"name": "output2", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 26}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "split",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "split",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 12 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output1", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 1}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1},
						{"name": "output2", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 26}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "altsplit",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "altsplit",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type", "name": "type", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 12 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output1", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 1}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1},
						{"name": "output2", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 26}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "altmerge",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "altmerge",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 1 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 12}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "zipper",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "zipper",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 1 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 12}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "compose",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "compose",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 1 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 12}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "constantStream",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "constantStream",
					"fields": [
						{"type": "type", "inputParams": {"label": "", "name": "label", "wirable": false, "id": "combLabel" }},
						{"type": "type", "inputParams": {"label": "Const", "name": "constant", "wirable": false, "id": "combin", typeInvite: "Starting value" }},
						{"type":"boolean", "inputParams": {"label": "EStream", "name": "estream", "id": "chkbox", "value":"false"}} 
					],
					"terminals": [
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 30}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "seededStream",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "seededStream",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "combLabel" }}
						//{"type": "type", "inputParams": {"label": "Type1", "name": "type1", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }},
						//{"type": "type", "inputParams": {"label": "Type2", "name": "type2", "wirable": false, "id": "type", typeInvite: "'blank' = no type" }}
					],
					"terminals": [
						{"name": "input1", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 1 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "input2", "direction": [0,-1], "offsetPosition": {"left": -15, "top": 26 },"ddConfig": {"type": "pipeIn","allowedTypes": ["pipeOut"]}, "nMaxWires": 10 },
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 12}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
					]
				}
			},
			{  
				"name": "counterStream",
				"container": {
					"xtype": "WireIt.FormContainer",
			  		"ddHandleClassName": "combinator",
					"title": "counterStream",
					"fields": [
						{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "combLabel" }},
						{"type": "type", "inputParams": {"label": "Count", "name": "count", "wirable": false, "id": "combin", typeInvite: "Starting value" }},						
						{"type":"boolean", "inputParams": {"label": "EStream", "name": "estream", "id": "chkbox", "value":"false"}} 
					],
					"terminals": [
						{"name": "output", "direction": [0,1], "alwaysSrc":"true", "offsetPosition": {"right": -15, "top": 30}, "ddConfig": {"type": "pipeOut","allowedTypes": ["pipeIn"]},"nMaxWires": 1}
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
		this.editor = new stream.WiringEditor(this.language);
		
		//editor.accordionView.openPanel(1);
   },
   
   /**
    * Run will go through all the wires/modules active in the gui and based on how we interpret the data
    * we will generate the haskell code and output it to a pop that will displat the code to the user.
    * Based on stream model.
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
   	  
	  // Variables
   	  var funs = "";
   	  var inArgs = "";
	  var codeTxt = "";
   	  var comment = "";
   	  var makeRed = "";
   	  var makeDel = "";
   	  var makeSel = "";
	  var inputTxt = "";
   	  var helpFuns = "";
   	  var outputTxt = "";
   	  var labelIndex = 0;
   	  var mainTxt = "";
   	  var makeSubFlow = "";
  	  var subflowCount = 0;
   	  var makeFunction = "";
   	  var liftTxt = "";
   	  var ltRTxt = "";
   	  var dupTxt = "";
   	  var splitTxt = "";
   	  var altsplitTxt = "";
   	  var altmergeTxt = "";
   	  var zipperTxt = "";
   	  var composeTxt = "";
   	  var constantStreamTxt = "";
   	  var seedStrTxt = "";
   	  var counterStreamTxt = "";
   	  var streamSel = "toPStream";
   	  var streamFlag = false;
   	  
   	  var wireArr = new Array();
   	  
   	  // Headers required in stream
   	  var header =  "\n"
			   + "import WF.Primitives \n" 
			   + "import WF.Types \n"
			   + "import Maybe \n\n";
	  
	  for (var i in wires) {
		 wireArr.push({name: "wire", value: "wire"+i});
  	  }
  	  
  	  // Cycle through activities (modules)
  	  for (var i in modules) {  	  	
		// Give every module a unique label by default if label not supplied
		if(modules[i].value.label == "")	{
			modules[i].value.label = "label" + labelIndex;
			labelIndex++;
		}		
		
		var labelTxt = modules[i].value.label;
    	  
		switch (modules[i].name) {
			
			// Get all the comment boxes
			case "comment": comment += "-- " + modules[i].value.comment + "\n"; break;

			// Get all helper functions
			case "helperfuns" : funs += modules[i].value.helper + " \n"; break;

			// Get the input arguments override, so that user can specify input arguments
			case "inargs" : inArgs = modules[i].value.inargs; break;

			// Get the main body
			case "main" : mainTxt += checkTxt(modules[i].value.main,"main",labelTxt); 
						  if(mainTxt.indexOf("EStream") != -1) {
						  	streamFlag = true;
						  }
						  break;
			
			// For lifter use the label and function code to create for eg: fun1 = lifter (\x -> x)
			// and create the lift line of code, output = liftlabel input
			case "lifter": //var typeTxt = (modules[i].value.type == "") ? "" : " :: " + streamSel + " " + modules[i].value.type; 
			               makeFunction += labelTxt + " = lifter " + checkTxt(modules[i].value.Code,"lifter",labelTxt) + "\n"; 
						   liftTxt += "    " + "lifter1ph1" + labelTxt + " = " + streamSel + " $ " + labelTxt + " lifter1ph2" + labelTxt + " \n";
						   break;

			// For lifter2 use the label and function code to create for eg: fun1 = lifter2 (\x -> x)
			// and create the lift line of code, output = liftlabel input1 input2
			case "lifter2": //var typeTxt1 = (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			                //var typeTxt2 = (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			                makeFunction += labelTxt + " = lifter2 " + checkTxt(modules[i].value.Code,"lifter2",labelTxt) + "\n"; 
						    liftTxt += "    " + "lifter2ph1" + labelTxt + " = " + streamSel + " $ " + labelTxt + " lifter2ph2" + labelTxt + " lifter2ph3" + labelTxt + " \n";
						    break;
						    
			// For lifter3 use the label and function code to create for eg: fun1 = lifter3 (\x -> x)
			// and create the lift line of code, output = liftlabel input1 input2 input3
			case "lifter3": //var typeTxt1 =  (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			                //var typeTxt2 =  (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			                //var typeTxt3 =  (modules[i].value.type3 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type3;
			                makeFunction += labelTxt + " = lifter3 " + checkTxt(modules[i].value.Code,"lifter3",labelTxt) + "\n"; 
						    liftTxt += "    " + "lifter3ph1" + labelTxt + " = " + streamSel + " $ " + labelTxt + " lifter3ph2" + labelTxt + " lifter3ph3" + labelTxt + " lifter3ph4" + labelTxt + " \n";
						    break;
			
			// For selectLtR, setup with placeholder text the selectLtR code line, eg: output = selectLtR input1 input2
			case "selectLtR": //var typeTxt1 = (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			                  //var typeTxt2 = (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			                  ltRTxt += "    " + "selectLtRph1" + labelTxt + " = " + streamSel + " $ " + "selectLtR selectLtRph2" + labelTxt + " selectLtRph3" + labelTxt + " \n"; 
			                  break;

			// For dup, setup with placeholder text the dup code line, eg: (output1,output2) = dup input
			case "dup": //var typeTxt = (modules[i].value.type == "") ? "" : " :: " + streamSel + " " + modules[i].value.type;
			            dupTxt += "    " + "(dupph1" + labelTxt + ",dupph2" + labelTxt + ") = dup dupph3" + labelTxt + " \n"; 
			            break;
			         
			// For split, setup with placeholder text the split code line, eg: (output1,output2) = split input
			case "split": //var typeTxt = (modules[i].value.type == "") ? "" : " :: " + streamSel + " " + modules[i].value.type;
			              splitTxt += "    " + "(splitph1" + labelTxt + ",splitph2" + labelTxt + ") = split splitph3" + labelTxt + " \n"; 
			              break;
			         
			// For altsplit, setup with placeholder text the altsplit code line, eg: (output1,output2) = altsplit input
			case "altsplit": //var typeTxt = (modules[i].value.type == "") ? "" : " :: " + streamSel + " " + modules[i].value.type;
			                 altsplitTxt += "    " + "(altsplitph1" + labelTxt + ",altsplitph2" + labelTxt + ") = altsplit altsplitph3" + labelTxt + " \n"; 
			                 break;

			// For altmerge, setup with placeholder text the altmerge code line, eg: output = altmerge input1 input2
			case "altmerge": //var typeTxt1 = (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			                 //var typeTxt2 = (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			                 altmergeTxt += "    " + "altmergeph1" + labelTxt + " = " + streamSel + " $ " + "altmerge altmergeph2" + labelTxt + " altmergeph3" + labelTxt + " \n"; 
			                 break;

			// For zipper, setup with placeholder text the zipper code line, eg: output = zipper input1 input2
			case "zipper": //var typeTxt1 = (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			               //var typeTxt2 = (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			               zipperTxt += "    " + "zipperph1" + labelTxt + " = " + streamSel + " $ " + "zipper zipperph2" + labelTxt + " zipperph3" + labelTxt + " \n"; 
			               break;

			// For compose, setup with placeholder text the compose code line, eg: output = compose input1 input2
			case "compose": //var typeTxt1 = (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			                //var typeTxt2 = (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			                composeTxt += "    " + "composeph1" + labelTxt + " = " + streamSel + " $ " + "compose composeph2" + labelTxt + " composeph3" + labelTxt + " \n"; 
			                break;
			
			// For seededStream, setup with placeholder text the seededStream code line, eg: output = seededStream input1 input2
			case "seededStream": //var typeTxt1 = (modules[i].value.type1 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type1;
			                     //var typeTxt2 = (modules[i].value.type2 == "") ? "" : " :: " + streamSel + " " + modules[i].value.type2;
			                     seedStrTxt += "    " + "seededStreamph1" + labelTxt + " = " + streamSel + " $ " + "seededStream seededStreamph2" + labelTxt + " seededStreamph3" + labelTxt + " \n"; 
			                     break;

			default:
		}
  	  }
  	  
	  // Cycle through wires and for each wire replace the src and tgt placeholder text with the wire name,
	  // wires are used as variables to connect the stream workflow logic
	  // wires[n].src.moduleId - returns the wire's source module id
	  // modules[n] - get all the properties of the n module
  	  for (var i in wires) {  
		var srclabelTxt = modules[wires[i].src.moduleId].value.label;
		var tgtlabelTxt = modules[wires[i].tgt.moduleId].value.label;
		var srcName = modules[wires[i].src.moduleId].name;
		var tgtName = modules[wires[i].tgt.moduleId].name;
		var wireTxt = wireArr[i].value;			
		
		// For all sources replace placeholder text with wire name
		switch(srcName) {
			// Setup variables for all inputs
			case "input": inputTxt += "    " + wireTxt + " = " + modules[wires[i].src.moduleId].value.input + " \n"; 
						  break;
			
			case "lifter": liftTxt = liftTxt.replace("lifter1ph1" + srclabelTxt,wireTxt);  break;
			case "lifter2": liftTxt = liftTxt.replace("lifter2ph1" + srclabelTxt,wireTxt);  break;
			case "lifter3": liftTxt = liftTxt.replace("lifter3ph1" + srclabelTxt,wireTxt);  break;
			
			case "selectLtR": ltRTxt = ltRTxt.replace("selectLtRph1" + srclabelTxt,wireTxt);  break;
			
			case "dup": if(wires[i].src.terminal == "output1") {
			            	dupTxt = dupTxt.replace("dupph1" + srclabelTxt,wireTxt);
			            } else {
			            	dupTxt = dupTxt.replace("dupph2" + srclabelTxt,wireTxt);
			            }
			            break;
			
			case "split": if(wires[i].src.terminal == "output1") {
			            	splitTxt = splitTxt.replace("splitph1" + srclabelTxt,wireTxt);
			            } else {
			            	splitTxt = splitTxt.replace("splitph2" + srclabelTxt,wireTxt);
			            }
			            break;
			
			case "altsplit": if(wires[i].src.terminal == "output1") {
			            	altsplitTxt = altsplitTxt.replace("altsplitph1" + srclabelTxt,wireTxt);
			            } else {
			            	altsplitTxt = altsplitTxt.replace("altsplitph2" + srclabelTxt,wireTxt);
			            }
			            break;
			            
			case "altmerge": altmergeTxt = altmergeTxt.replace("altmergeph1" + srclabelTxt,wireTxt);  break;
			case "zipper": zipperTxt = zipperTxt.replace("zipperph1" + srclabelTxt,wireTxt);  break;
			case "compose": composeTxt = composeTxt.replace("composeph1" + srclabelTxt,wireTxt);  break;			
						
			case "constantStream": if(modules[wires[i].src.moduleId].value.estream) { streamSel = "toEStream"; }
								   constantStreamTxt += "    " + wireTxt + " = " + streamSel + " $ " + "constantStream " + modules[wires[i].src.moduleId].value.constant + " \n";  
								   break;
		
			case "seededStream": seedStrTxt = seedStrTxt.replace("seededStreamph1" + srclabelTxt,wireTxt);  break;
			
			case "counterStream": if(modules[wires[i].src.moduleId].value.estream) { streamSel = "toEStream"; }
			                      counterStreamTxt += "    " + wireTxt + " = " + streamSel + " $ " + "counterStream " + modules[wires[i].src.moduleId].value.count + " \n";  
			                      break;
			
			default:			
		}
		
		// For all targets replace placeholder text with wire name
		switch(tgtName) {
			// This will be the variable that is returned at the end, should be only 1
			case "output": outputFun = modules[wires[i].tgt.moduleId].value.output;
						   outputTxt += ((outputFun == "") ? "" : outputFun + " ") + wireTxt + " \n"; 
						   break;
			            
			case "lifter": liftTxt = liftTxt.replace("lifter1ph2" + tgtlabelTxt,wireTxt);  break;
			
			case "lifter2": if(wires[i].tgt.terminal == "input1") {
			            		liftTxt = liftTxt.replace("lifter2ph2" + tgtlabelTxt,wireTxt);
			            	} else {
			            		liftTxt = liftTxt.replace("lifter2ph3" + tgtlabelTxt,wireTxt);
			            	}
			            	break;
			
			case "lifter3": if(wires[i].tgt.terminal == "input1") {
			            		liftTxt = liftTxt.replace("lifter3ph2" + tgtlabelTxt,wireTxt);
			            	} else if(wires[i].tgt.terminal == "input2") {
			            		liftTxt = liftTxt.replace("lifter3ph3" + tgtlabelTxt,wireTxt);
			            	} else {
			            		liftTxt = liftTxt.replace("lifter3ph4" + tgtlabelTxt,wireTxt);
			            	} 
			            	break;
	
			case "selectLtR": if(wires[i].tgt.terminal == "input1") {
			            			ltRTxt = ltRTxt.replace("selectLtRph2" + tgtlabelTxt,wireTxt);
			            		 } else {
			            			ltRTxt = ltRTxt.replace("selectLtRph3" + tgtlabelTxt,wireTxt);
			            		 }
			            		 break;
			
			case "dup": dupTxt = dupTxt.replace("dupph3" + tgtlabelTxt,wireTxt);  break;
			case "split": splitTxt = splitTxt.replace("splitph3" + tgtlabelTxt,wireTxt);  break;
			case "altsplit": altsplitTxt = altsplitTxt.replace("altsplitph3" + tgtlabelTxt,wireTxt);  break;
			
			case "altmerge": if(wires[i].tgt.terminal == "input1") {
			            		altmergeTxt = altmergeTxt.replace("altmergeph2" + tgtlabelTxt,wireTxt);
			            	} else {
			            		altmergeTxt = altmergeTxt.replace("altmergeph3" + tgtlabelTxt,wireTxt);
			            	}
			            	break;
			
			case "zipper": if(wires[i].tgt.terminal == "input1") {
			            		zipperTxt = zipperTxt.replace("zipperph2" + tgtlabelTxt,wireTxt);
			            	} else {
			            		zipperTxt = zipperTxt.replace("zipperph3" + tgtlabelTxt,wireTxt);
			            	}
			            	break;
			
			case "compose": if(wires[i].tgt.terminal == "input1") {
			            		composeTxt = composeTxt.replace("composeph2" + tgtlabelTxt,wireTxt);
			            	} else {
			            		composeTxt = composeTxt.replace("composeph3" + tgtlabelTxt,wireTxt);
			            	}
			            	break;
	
			case "seededStream": if(wires[i].tgt.terminal == "input1") {
			            			seedStrTxt = seedStrTxt.replace("seededStreamph2" + tgtlabelTxt,wireTxt);
			            		 } else {
			            			seedStrTxt = seedStrTxt.replace("seededStreamph3" + tgtlabelTxt,wireTxt);
			            		 }
			            		 break;
			
			default:			
		}

	  }
	  
  	  // Sets up the text with the initial functions
  	  codeTxt += funs + "\n" + makeFunction + " \n";
  	  
  	  codeTxt += workflowName + " = \n  let \n" + inputTxt + counterStreamTxt + seedStrTxt + ltRTxt + liftTxt + dupTxt;
  	  codeTxt += constantStreamTxt + splitTxt + altsplitTxt + altmergeTxt + zipperTxt + composeTxt + "  in " + outputTxt;
   	  
  	  // If input arguments are supplied it will replace the default workflow inargs variable with the supplied input arguments
  	  if(inArgs != "")	{
  	  	  codeTxt = codeTxt.replace("inargs",inArgs);
  	  }
  	  
 	  // Check whether workflow is a module or not
  	  var moduleFlag = false;
  	  
  	  // If no main code is supplied it will turn the workflow into a module only
  	  if(mainTxt != "")	{  	  
		  // Create main with give result text
		  codeTxt = comment + "\n\n{-# OPTIONS_GHC -fglasgow-exts #-}" + "\nmodule " + workflowName.toUpperCase() + " where" + header + codeTxt + "\n" + mainTxt;
	  } else {
	  	  codeTxt = comment + "-- This is a module\n\n{-# OPTIONS_GHC -fglasgow-exts #-}" + "\nmodule " + workflowName.toUpperCase() + " where" + header + codeTxt;
	  	  moduleFlag = true;
	  }
	  
	  // If user types EStream in mail then we use EStream, else we stick to default PStream
	  if(streamFlag) {
	  	  codeTxt = codeTxt.replace(/toPStream/g,"toEStream");
	  }
	  
	  // Replace workflow with workflowName+workflow. Also make the workflowName all uppercase.
	  // This is done so that workflows can be used in other workflows as subflows.
	  // workflow variable is no longer generic but is now title+workflow
	  codeTxt = codeTxt.replace("stream",workflowName + "stream");
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
	  
	  // Pops up with the code displayed to the user.
	  // Checks if there are errors and if there are it will ouput that instead of the code.
	  // Also checks if there are modules present or not, if not it will output no workflows present.
	  // Also pass name of workflow which will be used as the filename.
	  if(modules.length != 0)	{
	    if(error == "") {
	  		showPopup(500,700,codeTxt,escapeHtml(numCodeTxt),workflowName + ".hs",moduleFlag);
	  	} else {
	  		showPopup(500,700,"","--- Error Log ---\n\n" + error + "\n-----------------",workflowName + ".hs",moduleFlag);
	  	}
	  } else {
	  	showPopup(500,700,"","",workflowName + ".hs",moduleFlag);	  
	  }
	  
	  // Clears error log
	  error = "";	  
	},
   
   /**
    * Debug - For step by step running of stream code
    * @method debug
    * @static
    */
   debug: function() {
   }
};

/**
 * The wiring editor is overriden to add buttons to the control bar
 */
stream.WiringEditor = function(options) {
   stream.WiringEditor.superclass.constructor.call(this, options);
};

YAHOO.lang.extend(stream.WiringEditor, WireIt.WiringEditor, {
   
   /**
    * Add the buttons, run, runs, debug
    */
   renderButtons: function() {
      stream.WiringEditor.superclass.renderButtons.call(this);
      var toolbar = YAHOO.util.Dom.get('toolbar');
      var runButton = new YAHOO.widget.Button({ label:"Generate", id:"WiringEditor-runButton", container: toolbar });
      runButton.on("click", stream.run, stream, true);     
      //var runButton3 = new YAHOO.widget.Button({ label:"Debug", id:"WiringEditor-debugButton", container: toolbar });
      //runButton3.on("click", stream.debug, stream, true);
   },
   
   	/**
   	 * Customize the load success handler for the composed module list
   	 */
   	onLoadSuccess: function(wirings) {
   		stream.WiringEditor.superclass.onLoadSuccess.call(this,wirings);
   	
   		//  Customize to display composed module in the left list
   		this.updateComposedModuleList();
	},
   
   /**
    * Overwrite updateLoadPanelList to add Composed modules to the module list
    * This will be used as subflows for stream
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
					"xtype": "stream.ComposedContainer",
			  		"ddHandleClassName": "subactivity",
					"title": "stream"
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
 * This is used for the subflows in stream. Depending on the number of inputs and outputs to the subflow
 * the terminals will be generated. For a subflow you will only see a label and terminals
 * @class ComposedContainer
 * @extends WireIt.inputExContainer
 * @constructor
 */
stream.ComposedContainer = function(options, layer) {
   
   if(!options.fields) {
      
      options.fields = [{"type": "type", "inputParams": {"label": "Label", "name": "label", "wirable": false, "id": "subfllab", "value": options.title + "workflow" }}];
      options.terminals = [];
      var terIn = 0;
      var terOut = 0;
      var inNamCnt = 0;
      var outNamCnt = 0;
      
      var pipe = stream.editor.getPipeByName(options.title);
      
      for(var i = 0 ; i < pipe.modules.length ; i++) {
         var m = pipe.modules[i];
         if( m.name == "input") {
         	terIn++;
         }
         else if(m.name == "output") {
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
               "offsetPosition": {"top": terIn, "left": -15}, 
               "ddConfig": {
                   "type": "pipeIn",
                   "allowedTypes": ["pipeOut"],
                   "nMaxWires": 1
                }
            });
            terIn += 20;
            inNamCnt++;
         }
         else if(m.name == "output") {
            options.terminals.push({
               name: "output" + outNamCnt,
               "direction": [0,1],
               "alwaysSrc":"true",
               "offsetPosition": {"top": terOut, "right": -15}, 
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
   
   stream.ComposedContainer.superclass.constructor.call(this, options, layer);
};

YAHOO.extend(stream.ComposedContainer, WireIt.FormContainer, {
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
    } else if(codeTxt.indexOf("import System") != -1) {
    	error += "Cannot use System in the code";
    } else if(codeTxt.indexOf("import Network") != -1) {
    	error += "Cannot use Network in the code";
    } else if(codeTxt.indexOf("import qualified System") != -1) {
    	error += "Cannot use System in the code";
    } else if(codeTxt.indexOf("import qualified Network") != -1) {
    	error += "Cannot use Network in the code";
    }   
	return codeTxt;
}