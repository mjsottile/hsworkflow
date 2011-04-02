--- STREAM 2.0 README ---

This GUI allows a user to visualize workflows based on Matt Sottile and Geoff Hulette's STREAM model.

Drawn workflows can then generate haskell code that can be executed server-side with the output displayed in a popup.

This is for demo purposes only.

Modules: On the left, they are the components you will need to build a workflow.

Center Layer: Drag and drop modules, connect modules, all of that will be done in the center layer

Properties: On the right, properties of a workflow.

    * Title - title of workflow, filename, workflow variable name = title+"workflow".
    * Description - Description of workflow.


Top Layer: Create a new workflow, load workflow, save workflow, delete workflow, open this window.

    * Generate - Generates code based on the workflow drawn in the center layer.

GUI location: /gui/stream/

You can navigate to it through a browser without needing
to install the db.

If you want to install the db, the files are located in: /gui/backend/

Wireit: README.txt, VERSION.txt, license.txt