import sys
from editor import Editor
from PyQt4 import QtGui, QtCore
from PyQt4.Qsci import QsciScintilla, QsciScintillaBase, QsciLexerPython



class App(QtGui.QMainWindow):

    def __init__(self, width, height, parent = None):
        QtGui.QMainWindow.__init__(self,parent)
        self.width = width
        self.height = height
        self.init_ui()

    def init_ui(self):

        self.setGeometry(0,0,self.width,self.height)

        self.setWindowTitle("OmegaIDE")
        self.text = Editor()
        self.text.cursorPositionChanged.connect(self.show_cursor_pos)

        self.setCentralWidget(self.text)

        self.init_file_manager()

        self.init_toolbar()
        self.init_formatbar()
        self.init_menubar()

        self.status_bar = self.statusBar()

    def init_formatbar(self):

        self.formatbar = self.addToolBar("Format")

    def init_menubar(self):

      menubar = self.menuBar()

      file = menubar.addMenu("File")
      edit = menubar.addMenu("Edit")
      view = menubar.addMenu("View")
      prefs = menubar.addMenu("Preferences")
      project = menubar.addMenu("Project")

    def init_toolbar(self):

        self.save_file_action = QtGui.QAction(QtGui.QAction("&Save",self))
        self.save_file_action.setIcon(QtGui.QIcon("icons/save.png"))
        self.save_file_action.setStatusTip("Save this file.")
        self.save_file_action.setShortcut("Ctrl+S")
        self.save_file_action.triggered.connect(self.save_file)
        

        self.new_file_action = QtGui.QAction(QtGui.QAction("&New",self))
        self.new_file_action.setIcon(QtGui.QIcon("icons/new.png"))
        self.new_file_action.setStatusTip("Create a new file in this project.")
        self.new_file_action.setShortcut("Ctrl+N")
        self.new_file_action.triggered.connect(self.make_new_file)


        self.open_file_action = QtGui.QAction(QtGui.QAction("&Open",self))
        self.open_file_action.setIcon(QtGui.QIcon("icons/open.png"))
        self.open_file_action.setStatusTip("Open a file.")
        self.open_file_action.setShortcut("Ctrl+O")
        self.open_file_action.triggered.connect(self.open_file)

        self.open_project_action = QtGui.QAction(QtGui.QAction("&Open Project",self))
        self.open_project_action.setIcon(QtGui.QIcon("icons/project.png"))
        self.open_project_action.setStatusTip("Open a project.")
        self.open_project_action.setShortcut("Ctrl+P")
        self.open_project_action.triggered.connect(self.open_project)


        self.new_project_action = QtGui.QAction(QtGui.QAction("&New Project",self))
        self.new_project_action.setIcon(QtGui.QIcon("icons/new_project.png"))
        self.new_project_action.setStatusTip("Create a new project.")
        self.new_project_action.setShortcut("Ctrl+Shift+P")
        self.new_project_action.triggered.connect(self.new_project)

       


        self.main_toolbar = self.addToolBar("Options")
        self.main_toolbar.setStyleSheet('QToolBar{spacing:5px;}')
        self.main_toolbar.addAction(self.save_file_action)
        self.main_toolbar.addAction(self.new_file_action)
        self.main_toolbar.addAction(self.open_file_action)
        self.main_toolbar.addAction(self.open_project_action)
        self.main_toolbar.addAction(self.new_project_action)
        
        self.addToolBarBreak()


    def save_file(self):
        if self.file == "":
            self.file = QtGui.QFileDialog.getSaveFileName(self, 'Save File')

        with open(self.file,"wt") as file:
            file.write(self.text.get_plain_text())#toPlainText())

    def make_new_file(self):
        pass

    def open_file(self):

        if self.file == "":
            self.file = QtGui.QFileDialog.getOpenFileName(self, "Open", ".", "")

        with open(self.file, "rt") as file:
            self.text.setText(file.read())

        self.text

    def open_project(self):
        print("Project Opened")

    def new_project(self):
        print("New Project")

    def init_file_manager(self):
        self.file = ""

    def show_cursor_pos(self):
        pass
        # c = self.text.textCursor()

        # line = c.blockNumber() + 1 #MERE MORTALS LIKE THEIR LINES TO BEGIN AT 1
        # col = c.columnNumber() + 1 #MERE MORTALS LIKE THEIR COLS TO BEGIN AT 1

        # self.status_bar.showMessage("{}::{}".format(line,col))

def main():

    app = QtGui.QApplication(sys.argv)

    main = App(1280,1024)
    main.showMaximized()

    sys.exit(app.exec_())

if __name__ == "__main__":
    main()