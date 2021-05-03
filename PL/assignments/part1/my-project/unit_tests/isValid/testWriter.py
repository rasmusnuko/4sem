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
  return "[({},{}),({},{}),({},{}),({},{}),({},{})]".format(pieces[0][0], pieces[0][1], pieces[1][0], pieces[1][1], pieces[2][0], pieces[2][1], pieces[3][0], pieces[3][1], pieces[4][0], pieces[4][1])

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
      result.append(card)
  return arrangeCards(result)

def performMove(cards, card, movePlayed, piecesA):
  # moving piece
  newPieces = piecesA
  newPieces[0][0] = (newPieces[0][0] + card[movePlayed][0])
  newPieces[0][1] = (newPieces[0][1] + card[movePlayed][1])
  # Making move string
  moveString = "(({},{}), ({},{}), \"{}\")\n".format(piecesA[0][0], piecesA[0][1], newPieces[0][0], newPieces[0][1], cards[0][0]) 
  # Rearranging cards
  newCards = []
  newCards.append(cards[4])
  if cards[0] == card:
      newCards.append(cards[1])
  else: 
      newCards.append(cards[0])
  newCards.extend(cards[2:4])
  newCards.append(card)
  newCards = arrangeCards(newCards)

  return (newPieces, moveString, newCards)

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
for card in allCards:
  for i in range(1,len(card)):
    piecesA = [[1,2], [0,0], [0,1], [0,3], [0,4]]
    piecesB = [[4,2], [4,0], [4,1], [4,3], [4,4]]
    cards = chooseCards(allCards, card[0])
    fileName = ("NUKO_" + card[0].lower() + str(i))
    f = open((fileName + ".in"), "w")   # .in file
    f.write(makeState(cards, piecesA, piecesB, 0))
    move = performMove(cards, card, i, piecesA)
    f.write(move[1])
    f = open((fileName + ".out"), "w")  # .out file
    f.write(makeState(move[2], move[0], piecesB, 1))
