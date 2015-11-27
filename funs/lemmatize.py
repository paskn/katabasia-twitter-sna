# Using pymystem3 lemmatize texts
import sys
from pymystem3 import Mystem

text = sys.argv[1]
m = Mystem()

lemmas = m.lemmatize(text)

print(''.join(lemmas))
