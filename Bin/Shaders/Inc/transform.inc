vec3 transformVector(vec3 n, mat4 mat){
  vec3 newNorm;
  newNorm.x = n.x * mat[0][0] + n.y * mat[1][0] + n.z * mat[2][0] + mat[3][0];
  newNorm.y = n.x * mat[0][1] + n.y * mat[1][1] + n.z * mat[2][1] + mat[3][1];
  newNorm.z = n.x * mat[0][2] + n.y * mat[1][2] + n.z * mat[2][2] + mat[3][2];
  return newNorm;
}