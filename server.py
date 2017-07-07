from bottle import Bottle, run, request, static_file
#basic frontend server to run the elm app for Tim
import sys

if(len(sys.argv) != 2):
    print "python server.py ROOT_DIR"
    sys.exit(0)


app = Bottle()
htmlroot = sys.argv[1]

@app.route('/<filepath:path>')
def server_static(filepath):
    response = static_file(filepath, root=htmlroot)
    response.set_header("Cache-Control", "public, max-age=0, no-cache, no-store, must-revalidate")
    response.set_header("Pragma", "no-cache");
    response.set_header("Expires", "0");
    return response

run(app, host='0.0.0.0', port=8080, debug=True)
