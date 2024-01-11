import sys
import asyncio
import json
import websockets

async def establish_websocket_connection(uri):
    try:
        # Increase the max_size parameter to handle larger messages
        websocket = await websockets.connect(uri, timeout=1000, max_size=2**50)  # Adjust the max_size as needed
        return websocket
    except (websockets.exceptions.ConnectionClosedError, asyncio.TimeoutError) as e:
        print(f"Error: {e} while establishing WebSocket connection")
        return None

async def get_block_data(websocket, block_number):
    try:
        # Construct the JSON-RPC request to get the block hash by block number
        request_hash = json.dumps({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "chain_getBlockHash",
            "params": [block_number]
        })

        # Send the request to get block hash
        await websocket.send(request_hash)

        # Receive the response
        response_hash = await websocket.recv()

        # Parse the JSON response
        block_hash_info = json.loads(response_hash)
        block_hash = block_hash_info.get("result", None)

        if not block_hash:
            return None

        # Construct the JSON-RPC request to get a specific block by hash
        request_block = json.dumps({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "chain_getBlock",
            "params": [block_hash]
        })

        # Send the request to get block data
        await websocket.send(request_block)

        # Receive the response
        response_block = await websocket.recv()

         # Parse the JSON response
        block_info = json.loads(response_block)

        # Calculate the number of extrinsics
        extrinsics_count = len(block_info.get("result", {}).get("block", {}).get("extrinsics", []))

        # Calculate the size in bytes occupied by extrinsics
        extrinsics_size = sum(len(json.dumps(extrinsic)) for extrinsic in block_info.get("result", {}).get("block", {}).get("extrinsics", []))

        return {
            "block_number": block_number,
            "block_hash": block_hash,
            "extrinsics_count": extrinsics_count,
            "extrinsics_size_bytes": extrinsics_size
        }

    except (websockets.exceptions.ConnectionClosedError, asyncio.TimeoutError) as e:
        print(f"Error: {e} while fetching data for block {block_number}")
        return None

async def fetch_and_save_block_data(start_block, end_block, websocket):
    block_data = []

    for block_number in range(start_block, end_block + 1):
        data = await get_block_data(websocket, block_number)
        if data:
            block_data.append(data)

    return block_data

async def main():
    uri = "wss://rpc.ibp.network/polkadot"

    # Establish WebSocket connection
    websocket = await establish_websocket_connection(uri)

    if websocket:
        # Set the block number range using command-line arguments
        start_block_number = int(sys.argv[1]) if len(sys.argv) > 1 else 18680000
        end_block_number = int(sys.argv[2]) if len(sys.argv) > 2 else 18680100

        # Run the event loop to fetch and save block data
        block_data = await fetch_and_save_block_data(start_block_number, end_block_number, websocket)

        # Print the block data
        print(json.dumps(block_data, indent=2))

        # Close the WebSocket connection
        await websocket.close()

# Run the event loop
asyncio.get_event_loop().run_until_complete(main())
