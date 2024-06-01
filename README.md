```haskell
ghci> raise Tone (Pitch A (Just Sharp))
B
ghci> raise Tone (Pitch D (Just Sharp))
F
ghci> raise Tone (Pitch C (Just Sharp))
D♯
ghci> raise Tone $ raise Tone (Pitch C (Just Sharp))
F
ghci> Pitch C Nothing
C
ghci> Pitch C (Just Sharp)
C♯
ghci> Pitch B (Just Flat)
B♭
ghci> raise Tone $ Pitch B (Just Flat)
C
ghci> raise Tone $ Pitch B Nothing
C♯
ghci> 
```
