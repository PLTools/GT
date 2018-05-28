@type ('name, 'term) abs = ('name, 'term) GT.pair with gmap;;

@type ('var, 'lam, 'abs) a_lam = Var of 'var | Abs of 'abs | App of 'lam * 'lam with gmap;;
