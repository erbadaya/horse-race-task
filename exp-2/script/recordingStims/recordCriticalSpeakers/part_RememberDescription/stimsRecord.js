// Stims to record, descriptions

var fillerRaces = [
{'horse' : 'WaveRider', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Wave Rider_F1.wav', race: 9, type: 'distractor', 'description': 'Wave Rider broke hearts late after dropping in class.'}, 
{'horse' : 'Silversky', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Silversky_F1.wav', race: 9, type: 'distractor', 'description': 'Silversky kicked off in his handicap career in the last race.'}, 
{'horse' : 'SilverPhantom', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Silver Phantom_F1.wav', race: 9, type: 'distractor', 'description': 'Silver Phantom heard the clock tick in the last furlong.'}, 
{'horse' : 'RedMeridian', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Red Meridian_F1.wav', race: 9, type: 'distractor', 'description': 'Red Meridian caught the eye at the flats.'}, 
{'horse' : 'ObliqueRunner', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Oblique Runner_F1.wav', race: 10, type: 'distractor', 'description': 'Oblique runner switched yards, hitting rising ground on steel.'}, 
{'horse' : 'ElectricStride', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Electric Stride_F1.wav', race: 10, type: 'distractor', 'description': 'Electric Stride was by miller and came widest, always holding.'}, 
{'horse' : 'DesertFlower', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Desert Flower_F1.wav', race: 10, type: 'distractor', 'description': 'Desert Flower ran in snatches, still on the bridle.'}, 
{'horse' : 'CelestialQueen', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Celestial Queen_F1.wav', race: 10, type: 'distractor', 'description': 'Celestial Queen answered every call in a field of fillies.'}, 
{'horse' : 'TickerPike', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Ticker Pike_F1.wav', race: 11, type: 'distractor', 'description': 'Ticker Pike had it written beforehand.'}, 
{'horse' : 'GoldenTempest', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Golden Tempest_F1.wav', race: 11, type: 'distractor', 'description': 'Golden Tempest did mind the business end.'}, 
{'horse' : 'GildedLily', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Gilded Lily_F1.wav', race: 11, type: 'distractor', 'description': 'Gilded Lily charged to the line on the gallops.'}, 
{'horse' : 'AuricVale', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Auric Vale_F1.wav', race: 11, type: 'distractor', 'description': 'Auric Vale took the measure twice in first-time hood.'}, 
{'horse' : 'Stockman', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Stockman_F1.wav', race: 12, type: 'distractor', 'description': 'Stockman led them a merry dance after being pipped.'}, 
{'horse' : 'RoyalRebel', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Royal Rebel_F1.wav', race: 12, type: 'distractor', 'description': 'Royal Rebel was visored for the first time.'}, 
{'horse' : 'Lovelace', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Lovelace_F1.wav', race: 12, type: 'distractor', 'description': 'Lovelace was first-time hooded and worked the oracle.'}, 
{'horse' : 'SolarFlash', 'delivery' :  'fluent', 'nativeness' : 'native', 'speaker_id' : 'F1', 'audio' : 'Solar Flash_F1.wav', race: 12, type: 'distractor', 'description': 'Solar Flash had a bit to spare off in a quiet sit.'}
]

var fillerRacesHorses = [
    {'race' : '1', 'horse' : 'BoldPride', 'role' : 'Filler', 'description' : 'Bold Pride was from a shrewd yard and cruising.'},
    {'race' : '1', 'horse' : 'LightningMonarch', 'role' : 'Filler', 'description' : 'Lightning Monarch settled his own sums in the turf course.'},
    {'race' : '1', 'horse' : 'UrbanLegend', 'role' : 'Filler', 'description' : 'Urban Legend was in the mix with a generous weight.'},
    {'race' : '2', 'horse' : 'CrystalFrontier', 'role' : 'Filler', 'description' : 'Crystal Frontier settled well in midfield.'},
    {'race' : '2', 'horse' : 'EchoFury', 'role' : 'Filler', 'description' : 'Echo Fury held under a tender ride.'},
    {'race' : '2', 'horse' : 'SwiftValor', 'role' : 'Filler', 'description' : 'Swift Valor went through the bridle approaching the pole.'},
    {'race' : '3', 'horse' : 'DiamondHood', 'role' : 'Filler', 'description' : 'Diamond Hood found under minimal cajoling.'},
    {'race' : '3', 'horse' : 'Flint Sonata', 'role' : 'Filler', 'description' : 'Flint Sonata, wind op, had been suited by the cut in the ground.'},
    {'race' : '3', 'horse' : 'Vanya', 'role' : 'Filler', 'description' : 'Vanya reverted to the flat and the sand boded well.'},
    {'race' : '4', 'horse' : 'BraveHorizon', 'role' : 'Filler', 'description' : 'Brave Horizon had them stretched before the furlong pole.'},
    {'race' : '4', 'horse' : 'OnyxRune', 'role' : 'Filler', 'description' : 'Onyx Rune answered every question readily.'},
    {'race' : '4', 'horse' : 'StarDust', 'role' : 'Filler', 'description' : 'Star Dust is a fettle that showed plenty on the gallops.'},
    {'race' : '5', 'horse' : 'DarkMirage', 'role' : 'Filler', 'description' : 'Dark Mirage found the seam with first-time blinkers.'},
    {'race' : '5', 'horse' : 'Flashfire', 'role' : 'Filler', 'description' : 'Flashfire had a line to the judge being covered.'},
    {'race' : '5', 'horse' : 'SteadyFighter', 'role' : 'Filler', 'description' : 'Steady Fighter had afterburners, scrambling home the last furlong.'},
    {'race' : '6', 'horse' : 'BlackEmber', 'role' : 'Filler', 'description' : 'Black Ember had matters in safekeeping turning in.'},
    {'race' : '6', 'horse' : 'BrambleHalo', 'role' : 'Filler', 'description' : 'Bramble Hallo was known for sitting handy and traveling.'},
    {'race' : '6', 'horse' : 'RubyAsh', 'role' : 'Filler', 'description' : 'Ruby Ash had a stripped fitter for the reappearance.'},
    {'race' : '7', 'horse' : 'CrimsonEclipse', 'role' : 'Filler', 'description' : 'Crimson Eclipse was from an in-from yard and always covered.'},
    {'race' : '7', 'horse' : 'Vladimir', 'role' : 'Filler', 'description' : 'Vladimir was narrowly beaten in a fair maiden on the turf course.'},
    {'race' : '7', 'horse' : 'Worthing', 'role' : 'Filler', 'description' : 'Worthing flowed in the turf course running in blinkers.'},
    {'race' : '8', 'horse' : 'Emerald', 'role' : 'Filler', 'description' : 'Emerald was a picture in running at 7 furlongs.'},
    {'race' : '8', 'horse' : 'NimbusCrest', 'role' : 'Filler', 'description' : 'Nimbus Crest is a sharp sire who settled and saw it out.'},
    {'race' : '8', 'horse' : 'Windchaser', 'role' : 'Filler', 'description' : 'Windchaser had the edge without showing in the turfs.'}
]

var criticalHorses = [
    {'race' : '1', 'horse' : 'SaffronCrest', 'role' : 'Critical', 'description' : 'Saffron Crest was bred stout and asserted without stick.'},
    {'race' : '2', 'horse' : 'Nightfall', 'role' : 'Critical', 'description' : 'Nightfall proved over the trip after running green.'},
    {'race' : '3', 'horse' : 'PaleCorsair', 'role' : 'Critical', 'description' : 'Pale Corsair found plenty off the bridle.'},
    {'race' : '4', 'horse' : 'AirAce', 'role' : 'Critical', 'description' : 'Air Ace, gelded recently, made headway.'},
    {'race' : '5', 'horse' : 'Karerin', 'role' : 'Critical', 'description' : 'Karenin was placed and took it to the wire.'},
    {'race' : '6', 'horse' : 'BlueHorizon', 'role' : 'Critical', 'description' : 'Blue Horizon raced without the cheekpieces.'},
    {'race' : '7', 'horse' : 'SilentEclipse', 'role' : 'Critical', 'description' : 'Silent Eclipse travelled into the dip, digging deep.'},
    {'race' : '8', 'horse' : 'ScarletRacer', 'role' : 'Critical', 'description' : 'Scarlet Racer was a yard in form and picked up once angled.'}
]