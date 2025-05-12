import asyncio
import pandas as pd
from twscrape import API

usernames = [
        "Senator_Hurtado",
        "AlvaradoGilSD4",
        "SenDaveCortese",
        "SenMcNerney",
        "SenStevePadilla",
        "CASenCaballero",
        "SenatorSeyarto",
        "sencabaldon",
        "SenMeganDahle",
        "TimGraysonCA",
        "SFFCNorCal",
        "SenAishaWahab",
        "SenRogerNiello",
        "SenCervantes",
        "MoniqueLimonCA",
        "StricklandforCA",
        "Scott_Wiener",
        "BenAllenCA",
        "SenBlakespear",
        "SenatorChoi",
        "SenMariaEDurazo",
        "ShannonGroveCA",
        "SenJohnLaird",
        "SenatorMenjivar",
        "SenOchoaBogh",
        "SashaReneePerez",
        "Lola_Smallwood",
        "asmakilahweber",
        "SenatorAshby",
        "SenBobArchuleta",
        "JesseArreguin",
        "SenatorUmberg",
        "SenGonzalez33",
        "SenEloiseReyes",
        "SenJoshBecker",
        "SenSusanRubio",
        "SenHenryStern",
        "SenValladares",
        "SenBrianJones",
        "ilike_mike"
    ]

async def main():
    api = API("accounts.db")  # SQLite DB to store your session

    await api.pool.add_account(
        username="DataHack2025",
        password="DataHack2025!",  # required, even if unused
        email="tobyliu547@gmail.com",
        email_password="dontcare",
        cookies = "ct0=cdad9b104f8f2f54f094f99a901dd53a977d556010e25e3d95748e6901f07f6b919c085fd64d88ba3e9c521a299910181bfb23c8051b33c0a7d9fa52a60abd4cfcd968319264b8dcd12a9c56212d243a; auth_token=67e6ab856f675f2b280d7a73c6756f80fb40e0b1"
    )

    # Example usage: search for tweets
    all_tweets = []

    for username in usernames:
        try:
            user = await api.user_by_login(username)
            async for tweet in api.user_tweets(user.id, limit=20):
                all_tweets.append({
                    "username": tweet.user.username,
                    "date": tweet.date,
                    "content": tweet.rawContent
                })
        except Exception as e:
            print(f"Error fetching tweets for {username}: {e}")

    # Create a DataFrame and save to CSV
    df = pd.DataFrame(all_tweets)
    df.to_csv("california_politicians_tweets.csv", index=False)
    print("Tweets have been saved to california_politicians_tweets.csv")

if __name__ == "__main__":
    asyncio.run(main())
