#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const prettyJSONStringify = require('pretty-json-stringify');

try {
    if (!process.argv[2]) {
        throw new Error('Invalid input parameter');
    }

    const input = process.argv[2];
    console.log('Input: ', input);

    const output = `${path.basename(input, '.obj')}.json`;
    console.log('Output: ', output);

    const VERTEX_RE = /^v\s/;
    const NORMAL_RE = /^vn\s/;
    const TEXTURE_RE = /^vt\s/;
    const FACE_RE = /^f\s/;
    const MAT_RE = /^usemtl\s/;
    const WHITESPACE_RE = /\s+/;

    const Vertices = [];
    const Normals = [];
    const UVs = [];
    const unpacked = {
        Vertices: [],
        Normals: [],
        UVs: [],
        HashIndices: {},
        Surfaces: [],
        index: 0
    };
    let curIndexArray = 0;

    const data = fs.readFileSync(input).toString().split('\n');
    data.forEach((line) => {
        const elements = line.split(WHITESPACE_RE);
        elements.shift();

        if (VERTEX_RE.test(line)) {
            Vertices.push.apply(Vertices, elements);
        } else if (MAT_RE.test(line)) {
            const Material = line.split(' ')[1];
            unpacked.Surfaces.push({
                Material,
                Indices: []
            });
            curIndexArray = unpacked.Surfaces.length - 1;
        } else if (NORMAL_RE.test(line)) {
            Normals.push.apply(Normals, elements);
        } else if (TEXTURE_RE.test(line)) {
            UVs.push.apply(UVs, elements);
        } else if (FACE_RE.test(line)) {
            for (let j = 0, eleLen = elements.length; j < eleLen; j++) {
                if (elements[j] in unpacked.HashIndices) {
                    unpacked.Surfaces[curIndexArray].Indices.push(
                        unpacked.HashIndices[elements[j]]
                    );
                } else {
                    const vertex = elements[j].split('/');
                    // vertex position
                    console.log(Vertices[(vertex[0] - 1) * 3 + 0]);
                    if (Vertices[(vertex[0] - 1) * 3 + 0] == undefined){
                        throw new Error('Invalid index, try using --offset-zero flag.');
                    }
                    unpacked.Vertices.push(+Vertices[(vertex[0] - 1) * 3 + 0]);
                    unpacked.Vertices.push(+Vertices[(vertex[0] - 1) * 3 + 1]);
                    unpacked.Vertices.push(+Vertices[(vertex[0] - 1) * 3 + 2]);
                    // vertex textures
                    if (UVs.length) {
                        unpacked.UVs.push(+UVs[(vertex[1] - 1) * 2 + 0]);
                        unpacked.UVs.push(+UVs[(vertex[1] - 1) * 2 + 1]);
                    }
                    // vertex Normals
                    unpacked.Normals.push(+Normals[(vertex[2] - 1) * 3 + 0]);
                    unpacked.Normals.push(+Normals[(vertex[2] - 1) * 3 + 1]);
                    unpacked.Normals.push(+Normals[(vertex[2] - 1) * 3 + 2]);
                    // add the newly created vertex to the list of indices
                    unpacked.HashIndices[elements[j]] = unpacked.index;
                    unpacked.Surfaces[curIndexArray].Indices.push(unpacked.index);
                    // increment the counter
                    unpacked.index += 1;
                }
            }
        }
    });

    const mesh = {
        Surfaces: unpacked.Surfaces,
        Vertices: unpacked.Vertices,
        UVs: [],
        Normals: []
    };

    if (unpacked.UVs.length > 0) {
        mesh.UVs = unpacked.UVs;
    }
    if (unpacked.Normals.length > 0) {
        mesh.Normals = unpacked.Normals;
    }

    fs.writeFileSync(output, prettyJSONStringify(mesh, {
        spaceAfterComma: '',
        shouldExpand: (object, level, key) => {
            if (key === 'Surfaces') return true;
            if (key === 'Indices') return false;
            if (key === 'Vertices') return false;
            if (key === 'UVs') return false;
            if (key === 'Normals') return false;
            return true;
        }
    }));
} catch (e) {
    console.error(e);
}