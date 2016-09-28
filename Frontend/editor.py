#Editor
from PyQt4 import QtGui, Qsci

class Editor(Qsci.QsciScintilla):

	def __init__(self):
		Qsci.QsciScintilla.__init__(self)
		lexer = Qsci.QsciLexerPython(self)
		self.setLexer(lexer)