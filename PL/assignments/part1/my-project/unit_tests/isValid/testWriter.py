# Sort players' cards lexicographically
def arrangeCards(cards):
  p1Names = sorted([card[0] for card in cards[:2]])
  p2Names = sorted([card[0] for card in cards[2:4]])
  p1 = []
  for name in p1Names:
    for card in cards:
      if name == card[0]:
        p1.append(card)
        break
  p2 = []
  for name in p2Names:
    for card in cards:
      if name == card[0]:
        p2.append(card)
        break
  result = []
  result.extend(p1)
  result.extend(p2)
  result.append(cards[4])
  return result

def makeCards(cards):
  return "[\"{}\",\"{}\",\"{}\",\"{}\",\"{}\"]".format(cards[0][0], cards[1][0], cards[2][0], cards[3][0], cards[4][0])

def makePieces(pieces):
  return "[({},{})]".format(pieces[0][0], pieces[0][1])

def makeState(cards, piecesA, piecesB, turn):
  return "({},{},{},{})\n".format(makeCards(arrangeCards(cards)), makePieces(piecesA), makePieces(piecesB), turn)

def chooseCards(cards, card):
  result = []
  foundCard = 0
  for anyCard in cards:
    if card == anyCard:
       result.append(anyCard)
  for anyCard in cards:
    if card != anyCard and (len(result) < 5):
      result.append(anyCard)
  return result


def performMove(cards, card, movePlayed, piecesA):
  # Making move string
  moveString = "(({},{}), ({},{}), \"{}\")\n".format(piecesA[0][0], piecesA[0][1], (piecesA[0][0] + card[movePlayed][0]), (piecesA[0][1] + card[movePlayed][1]), cards[0][0]) 
  # Rearranging cards
  newCards = []
  newCards.append(cards[4])
  newCards.append(cards[1])
  newCards.extend(cards[2:4])
  newCards.append(cards[0])
  # moving piece
  newPieces = piecesA
  newPieces[0][0] = (piecesA[0][0] + card[movePlayed][0])
  newPieces[0][1] = (newPieces[0][1] + card[movePlayed][1])

  return (moveString, arrangeCards(newCards), newPieces)

# All cards and their moves
rabbit = ["Rabbit", (-1,-1), (1,1), (0,2)]
cobra = ["Cobra", (0,-1), (1,-1), (1,1)]
rooster = ["Rooster", (-1,-1), (0,-1), (0,1), (1,1)]
tiger = ["Tiger", (-1,0), (2, 0)]
monkey = ["Monkey", (-1,-1), (-1,1), (1,-1), (1,1)]
crab = ["Crab", (0,-2), (1,0), (0,2)]
crane = ["Crane", (-1,-1), (1,0), (-1,1)]
frog = ["Frog", (0,-2), (1,-1), (-1,1)]
boar = ["Boar", (0,-1), (0,1), (1,0)]
horse = ["Horse", (-1,0), (0,-1), (1,0)]
elephant = ["Elephant", (1,-1), (0,-1), (0,1), (1,1)]
ox = ["Ox", (0,1), (-1,0), (1,0)]
goose = ["Goose", (-1,1), (0,-1), (0,1), (1,-1)]
dragon = ["Dragon", (1,-2), (-1,-1), (-1,1), (1,2)]
mantis = ["Mantis", (1,-1), (-1,0), (1,1)]
eel = ["Eel", (1,-1), (-1,-1), (0,1)]
allCards = [rabbit, cobra, rooster, tiger, monkey, crab, crane, frog, boar, horse, elephant, ox, goose, dragon, mantis, eel]

# Creating .in & .out files
state = 0
for card in allCards:
  for i in range(1,len(card)):
    piecesA = [[1,2]]
    piecesB = [[4,2]]
    cards = chooseCards(allCards, card)
    fileName = ("MOVES_" + card[0].lower() + str(i))
    f = open((fileName + ".in"), "w")   # .in file
    state = makeState(cards, piecesA, piecesB, 0)
    f.write(state)
    move = performMove(cards, card, i, piecesA)
    f.write(move[0])
    f = open((fileName + ".out"), "w")  # .out file
    state = makeState(move[1], move[2], piecesB, 1)
    f.write(state)

print("Have a nice day (^:")
